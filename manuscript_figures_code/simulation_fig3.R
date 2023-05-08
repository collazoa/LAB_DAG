library(tidyverse)
# settings for styling visualization
cols <- RColorBrewer::brewer.pal(6, "Dark2")
########################################################
# setting number of random draws 
B=10000

########################################################
# setting parameter values 

# E Bernoulli prob_A
# L is normal with mean_L and sd_L
# Y = b_0 + b_1*E + b_2*L + rnorm(0,sd_Y)
# W = g_0 + g_1*E + g_2*L + rnorm(0, sd_W)

prob_A = 0.5          # probability of being assigned to treatment or control 
mean_L = 25           # initial infarct size 
sd_L = 5              # standard deviation of initial infarct size
b_0 = 50              # intercept value for final infarct size 
b_1 = c(0)            # treatment effect - here null-effect 
b_2 = 4               # effect of initial infarct size on final infarct size 
g_0 = 3               # intercept value of animal welfare W 
g_1 = c(-1,-3,-6)     # effect of treatment on animal welfare (E -> W) 
g_2 = c(-1)           # effect of initial infarct size on welfare (L -> W)
sd_Y = 10             # standard deviation of final infarct size Y  
sd_W = 2              # standard deviation of animal welfare W 
cutoff_W = c(0.2, 0.3,0.5) # attrition rates 
n = c(10, 20, 50) #total number of animals in the experiment



# combining all possible parameter values and values of n 
report <- expand_grid(prob_A, mean_L, 
                      sd_L, b_0, b_1, 
                      b_2, g_0, g_1, 
                      g_2, sd_Y, sd_W, 
                      cutoff_W, n)


######################################################################


######################################################################
# simulation of B random draws with set parameter values 

# preparation of empty matrix for model 1, model 2 & model 3 
# for later storage of values from replications 
m1 <- matrix(NA_real_, ncol=B, nrow=nrow(report))
m2 <- matrix(NA_real_, ncol=B, nrow=nrow(report))
m3 <- matrix(NA_real_, ncol=B, nrow=nrow(report))


# ensure reproducibility of random draw 
set.seed(10000)

######################################################################
# function for replication draws 

# here starts the counting of the row
for (i in 1:nrow(report)) {
  
  # here starts the loop in b
  for (b in 1:B) {
    
    # create the dataset
    dat <- data.frame(A = rep(0:1,report$prob_A[i]*report$n[i], each=1),
                      L = rnorm(n=report$n[i], mean=report$mean_L[i], 
                                sd=report$sd_L[i]))
    dat$W <- report$g_0[i] + report$g_1[i]*dat$A + report$g_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_W[i])
    dat$Y <- report$b_0[i] + report$b_1[i]*dat$A + report$b_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_Y[i])
    dat$S <- dat$W >= quantile(dat$W, probs=report$cutoff_W[i]) 
    dat_s <- dat %>% filter(S==TRUE)
    m2[i,b] =  mean(dat_s$Y[dat_s$A==1]) - mean(dat_s$Y[dat_s$A==0])
    
    m1[i,b] = mean(dat$Y[dat$A==1]) - mean(dat$Y[dat$A==0])
    
    model<-lm(Y ~ A + L, data = dat_s)
    m3[i,b]<-model$coefficients[2]
  }
}

################################################################################


# data transformation for stratification and visualization 

m1_tibble <- as_tibble(m1) %>% mutate(model = "m1", 
                                      index = 1:27)
m2_tibble <- as_tibble(m2) %>% mutate(model = "m2", 
                                      index = 1:27)
m3_tibble <- as_tibble(m3) %>% mutate(model = "m3", 
                                      index = 1:27)

df <- rbind(m1_tibble, m2_tibble, m3_tibble)

names<-NULL

for (i in 1:10000) {
  names[i] <- c(paste("estimate_",i, sep = ""))
}
colnames(df)<-c(names, "model", "index") 



df2 <- gather(df, 
              key  = sim_number, 
              value = effect_estimate, 
              estimate_1:estimate_10000)

df3 <- report %>% 
  mutate(index = 1:nrow(report))%>%
  select(c(index,n, cutoff_W, g_1))

df3$g_1 <- factor(df3$g_1,levels = c(-6, -3, -1), 
                  labels = c("major", "moderate", "minor"))
df3$cutoff_W <- factor(df3$cutoff_W, levels = c(0.2,0.3,0.5),
                       labels = c("20", "30", "50"))
df4 <- inner_join(df2, df3)

# write.csv(df4, "./manuscript_figures_code/df4.csv")

#######################################################################################
# stratified results
# 


# results for model 2 (biased sample) stratified by attrition rate 
r1<- df4%>%
  filter(model == "m2")%>%
  group_by(cutoff_W)%>%
  summarize(mean_bias = mean(effect_estimate, na.rm = T))
r1

# results for model 2 (biased sample) stratified by strength of negative side effects 

r2<- df4 %>%
  filter(model == "m2")%>%
  group_by(g_1)%>%
  summarize(mean_bias = mean(effect_estimate, na.rm = T))
r2

# results grouped by negative side-effects and attrition rates 
r3<- df4%>%
  filter(model == "m2")%>%
  group_by(g_1, cutoff_W)%>%
  summarize(mean_bias = mean(effect_estimate, na.rm = TRUE))
r3

r4<- df4%>%
  filter(model == "m2")%>%
  group_by(g_1, cutoff_W)%>%
  summarize(mean_bias = round(mean(effect_estimate, na.rm = TRUE),1),
            quan_25 = round(quantile(effect_estimate, probs = 0.025, na.rm = TRUE),1),
            quan_975 = round(quantile(effect_estimate, probs = 0.975, na.rm = TRUE),1))
r4

###########################################################################################
# Visualization 


# Figure 3  violin plot 

fig3<- ggplot(na.omit(df4))+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_violin(aes(x = g_1, y = effect_estimate, fill = model), alpha = 0.7)+
  facet_grid(cols = vars(factor(n)), 
             rows = vars(factor(cutoff_W)))+
  coord_cartesian(ylim = c(-40,40))+
  scale_fill_manual(values = c(cols[1], cols[4], cols[8]), 
                    labels = c("Model 1", "Model 2", "Model 3"))+
  labs(title = "Model comparison: adjustment for initial infarct size mitigates collider stratification bias",
       y = expression("difference in final infarct volume,mm"^"3"), 
       x = "strength of negative side-effect of treatment on welfare")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank())

fig3




####################################
#   Table S1

colnames(r4) <- c("side-effects", 
                  "total attrition rates (%)", 
                  "mean estimate", 
                  "2.5% quantile", 
                  "97.5% quantile")

kable(r4, 
      caption = "Table S1: Biased mean estimates of complete-case analysis under different scenarios of side-effects and attrition")%>%
  kable_classic()%>%
  save_kable("t1.png")



