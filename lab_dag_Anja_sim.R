library(tidyverse)
library(ggplot2)
install.packages("broom")
library(broom)
install.packages("kableExtra")
library(kableExtra)

prob_A = 0.5
mean_L = 190  # from O'Collins (Book Chapter) Table 20.6. infarct size in control cohorts -> However time point is not clear, should we rather use 
# values from Gerriet 2003 (Figure 4)? 
sd_L = 50
b_0 = 50
b_1 = 0 # this values will be varied to simulate a causal treatment effect 
b_2 = c(1.2) # this values were set according after combining estimates from Gerriet 2003 and O'Collins (Book Chapter): 
# baseline stroke volume is 6h after MCAo, outcome measurement is 24h after MCAo
g_0 = 3
g_1 = c(-10) #this values will be varied to simulate a treatment effect on welfare with different magnitudes  
g_2 = c(-9) #taken from Bederson et al. 1986 
sd_Y = 25
sd_W = 2
cutoff_W = c(0.3)
n = 20 #total number of animals in the experiment


report <- expand_grid(prob_A, mean_L, sd_L, b_0, b_1, b_2, g_0, g_1, g_2, sd_Y, sd_W, cutoff_W, n)
n_exp<-1000 #number of simulated experiments 

dat<-data.frame()
coef_biased<-rep(NA_real_, n_exp)
baseline_adj_coef<-rep(NA_real_, n_exp)
estimate_biased<-rep(NA_real_, n_exp)
estimate_unbiased<-rep(NA_real_, n_exp)


set.seed(100)

exploratory_data <- list()


for(b in 1: n_exp) {
  for ( i in 1:nrow(report)) {
    dat <- data.frame(A = rep(0:1,report$prob_A[i]*report$n[i], each=1),
                  L = rnorm(n=report$n[i], mean=report$mean_L[i], sd=report$sd_L[i]))
    dat$W <- report$g_0[i] + report$g_1[i]*dat$A + report$g_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_W[i])
    dat$Y <- report$b_0[i] + report$b_1[i]*dat$A + report$b_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_Y[i])
    dat$W2 <- dat$W >= quantile(dat$W, probs=report$cutoff_W[i]) 
    dat$A <- as.factor(dat$A)
    dat_s <- dat %>% filter(W2==TRUE)
    #estimate in the censored data-set
    estimate_biased[i] <-  mean(dat_s$Y[dat_s$A==1]) - mean(dat_s$Y[dat_s$A==0])
    estimate_unbiased[i]<- mean(dat$Y[dat$A ==1]- mean(dat$Y[dat$A == 0]))
    #perform a linear regression in the censored dataset
    #without correcting for baselin stroke volume 
    model1<-lm(dat_s$Y ~ dat_s$A, data = dat_s)
    coef_biased[i]<-model1$coefficients[2]
    #while correcting for baseline stroke volume (dat$L)
    model2<-lm(dat_s$Y ~ dat_s$A + dat_s$L, data = dat_s)
    baseline_adj_coef[i] <-model2$coefficients[2]

    report$coef_biased[i] <-coef_biased[i]
    report$baseline_adj_coef[i]<-baseline_adj_coef[i]
    report$estimate_biased[i] <-estimate_biased[i]
    report$estimate_unbiased[i]<-estimate_unbiased[i]
  }
  exploratory_data[[b]]<-report
}


#extraction of coefficients from simulation 
coef_biased<-list()

for (i in 1:length(exploratory_data)) {
    coef_biased[[i]]<-exploratory_data[[i]]$coef_biased[1]}

vec_coef_biased<-as.vector(unlist(coef_biased))


baseline_adj_coef<-list()

for (i in 1:length(exploratory_data)) {
  baseline_adj_coef[[i]]<-exploratory_data[[i]]$baseline_adj_coef[1]}

vec_baseline_adj_coef<-as.vector(unlist(baseline_adj_coef))


estimate_biased<-list()

for (i in 1:length(exploratory_data)) {
  estimate_biased[[i]]<-exploratory_data[[i]]$estimate_biased[1]}

vec_estimate_biased<-as.vector(unlist(estimate_biased))


estimate_unbiased<-list()

for (i in 1:length(exploratory_data)) {
  estimate_unbiased[[i]]<-exploratory_data[[i]]$estimate_unbiased[1]}

vec_estimate_unbiased<-as.vector(unlist(estimate_unbiased))




# creating dataframe from coefficient values 
estimates<-data.frame(baseline_adj_coef = vec_baseline_adj_coef, 
                      coef_biased = vec_coef_biased, 
                      estimate_biased = vec_estimate_biased, 
                      estimate_unbiased = vec_estimate_unbiased)

summary_estimates<-data.frame(diff_adj_unbiased = estimates$estimate_unbiased - estimates$baseline_adj_coef, 
                              diff_unadj_unbiased = estimates$estimate_unbiased - estimates$coef_biased)

mean_estimates<-data.frame(mean_diff_adj_unbiased = mean(summary_estimates$diff_adj_unbiased), 
                           mean_diff_unadj_unbiased = mean(summary_estimates$diff_unadj_unbiased))





#summary tables

summary_measures<-rbind(round(summary(estimates$coef_biased),1),
                        round(summary(estimates$baseline_adj_coef),1), 
                        round(summary(estimates$estimate_unbiased),1))

rownames(summary_measures)<-c("biased coefficient", "baseline-adjusted coefficient", "unbiased estimate")
kable(summary_measures)%>%kable_classic()


#visualization
#coefficient ranges 

range<-data.frame(rbind(round(summary(estimates$coef_biased),1),
                        round(summary(estimates$baseline_adj_coef),1), 
                        round(summary(estimates$estimate_unbiased),1)))%>%
                  select(c(2,3,5))%>%
                  mutate(coefficient = c("biased coefficient", "baseline-adjusted coefficient", "unbiased estimate"))%>%
                  rename(c("First", "Median", "Third", "coefficient"))

ggplot(range, aes(x= coefficient, y = Median)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymax = X3rd.Qu., ymin = X1st.Qu., width = 0.2))+ 
  geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.5, color = "red")+
  labs(y = "median and interquartile range of estimated coefficient ", 
       x = "")+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())



#converting estimates table to long format 
long_estimates<-gather(estimates, key = "coefficient", value = "value", c(baseline_adj_coef, coef_biased,estimate_unbiased))

#density plots for the estimates 

ggplot(long_estimates, aes(value, fill = coefficient)) + 
  geom_density(alpha = 0.2)+
  geom_vline(xintercept = c(-50,0, 50), linetype = c("longdash", "solid", "longdash"), color = c("grey", "black", "grey"), size = 1)+
  scale_fill_manual(values = c("#0072B2", "goldenrod1", "plum4"),
                      name = "coefficients", 
                      labels= c("baseline-adjusted censored dataset", "censored dataset", "complete dataset"))+
  labs(title = "Estimated values for treatment effect from simulated experiments",
       subtitle = "Coefficient value for treatment effect in complete sample, censored sample and baseline-adjusted censored sample",
       x= "coefficient value")+
  theme(legend.position = "bottom", 
        panel.background = element_blank(),
        axis.line = element_line())



#density plot: zoomed in to values x(-10,10)
  

ggplot(long_estimates, aes(value, color= coefficient)) + 
  geom_density(alpha = 0.2)+
  coord_cartesian(xlim = c(-5,5))+
  geom_vline(xintercept = c(mean(estimates$baseline_adj_coef),mean(estimates$coef_biased),mean(estimates$estimate_unbiased)),
             color = c("#0072B2", "goldenrod1", "plum4"))+
  scale_color_manual(values = c("#0072B2", "goldenrod1", "plum4"),
                    name = "coefficients", 
                    labels= c("baseline-adjusted censored dataset", "censored dataset", "complete dataset"))+
  theme(legend.position = "bottom", 
        panel.background = element_blank(),
        axis.line = element_line())+
  labs(title = "Estimated values for treatment effect from simulated experiments",
       subtitle = "Coefficient value for treatment effect in complete sample, censored sample and baseline-adjusted censored sample",
       x= "coefficient value")


