library(tidyverse)

B=10000

# A bernuoulli prob_A
# L is normal with mean_L and sd_L
# Y = b_0 + b_1*A + b_2*L + rnorm(0,sd_Y)
# W = g_0 + g_1*A + g_2*L + rnorm(0, sd_W)

prob_A = 0.5
mean_L = 190  # from O'Collins (Book Chapter) Table 20.6. infarct size in control cohorts -> However time point is not clear, should we rather use 
# values from Gerriet 2003 (Figure 4)? 
sd_L = 50
b_0 = 50
b_1 = c(0) # this values will be varied to simulate a causal treatment effect 
b_2 = c(1.2) # this values were set according after combining estimates from Gerriet 2003 and O'Collins (Book Chapter): 
# baseline stroke volume is 6h after MCAo, outcome measurement is 24h after MCAo
g_0 = 3
g_1 = c(-10,-60) #this values will be varied to simulate a treatment effect on welfare with different magnitudes  
g_2 = c(-9) #taken from Bederson et al. 1986 
sd_Y = 10
sd_W = 2
cutoff_W = c(0.3)
n = 20 #total number of animals in the experiment

report <- expand_grid(prob_A, mean_L, sd_L, b_0, b_1, b_2, g_0, g_1, g_2, sd_Y, sd_W, cutoff_W, n)

naive_estimate <- matrix(NA_real_, ncol=B, nrow=nrow(report))
adjusted_mod_estimate <- matrix(NA_real_, ncol=B, nrow=nrow(report))
no_exclusion_estimate <- matrix(NA_real_, ncol=B, nrow=nrow(report))

set.seed(5)

# here starts the counting of the row
for (i in 1:nrow(report)) {

# here starts the loop in b
for (b in 1:B) {

# create the dataset
dat <- data.frame(A = rep(0:1,report$prob_A[i]*report$n[i], each=1),
                  L = rnorm(n=report$n[i], mean=report$mean_L[i], sd=report$sd_L[i]))
dat$W <- report$g_0[i] + report$g_1[i]*dat$A + report$g_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_W[i])
dat$Y <- report$b_0[i] + report$b_1[i]*dat$A + report$b_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_Y[i])
dat$W2 <- dat$W >= quantile(dat$W, probs=report$cutoff_W[i]) 
dat_s <- dat %>% filter(W2==TRUE)
naive_estimate[i,b] =  mean(dat_s$Y[dat_s$A==1]) - mean(dat_s$Y[dat_s$A==0])
#interpretation: if values for naive_estimate are < 0 -> treatment is benefial bc it leads to lower infarct volumes at 
#time of outcome assessment 
no_exclusion_estimate[i,b] = mean(dat$Y[dat$A==1]) - mean(dat$Y[dat$A==0])
#interpretation: if values for no_exclusion_estimate are < 0 -> treatment is benefial bc it leads to lower infarct volumes at 
#time of outcome assessment
model<-lm(Y ~ A + L, data = dat_s)
adjusted_mod_estimate[i,b]<-model$coefficients[2]
}
}

# assess bias
apply(naive_estimate, 1, mean)
apply(no_exclusion_estimate, 1, mean)
apply(adjusted_mod_estimate, 1, mean)



#standard deviation
apply(naive_estimate, 1, sd)
apply(no_exclusion_estimate,1,sd)
apply(adjusted_mod_estimate, 1,sd)

df<-data.frame(mean_naive_estimate = round(apply(naive_estimate, 1, mean),2), 
           mean_no_exclusion_estimate = round(apply(no_exclusion_estimate, 1, mean),2),
           mean_adjusted_mod_estimate = round(apply(adjusted_mod_estimate, 1, mean),2), 
           side_effects = c("minor", "major"))

df_long<-df%>%gather(key = model, value = estimate, mean_naive_estimate:mean_adjusted_mod_estimate)

ggplot(df_long) + 
  geom_point(aes(x = side_effects, y = estimate, color = model))+
  scale_color_manual(values = c("#0072B2", "goldenrod1", "plum4"),
                     name = "coefficient for: treatment -> final infarct size",
                     labels= c("baseline-adjusted censored model", "censored model", "complete model"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  labs(title = "Estimated values for treatment effect from simulated experiments",
       subtitle = "Coefficient value for treatment effect in complete sample, censored sample and baseline-adjusted censored sample",
       x= "")+
  theme_classic()
  



range<-range%>% select(c(2,3,5))%>%
  mutate(coefficient = c("biased coefficient", "baseline-adjusted coefficient", "unbiased estimate"))

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




hist(naive_estimate)
hist(no_exclusion_estimate)
hist(adjusted_mod_estimate)
