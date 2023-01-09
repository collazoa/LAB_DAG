library(tidyverse)

B=1000


# A bernuoulli prob_A
# L is normal with mean_L and sd_L
# Y = b_0 + b_1*A + b_2*L + rnorm(0,sd_Y)
# W = g_0 + g_1*A + g_2*L + rnorm(0, sd_W)

prob_A = 0.5
mean_L = 25  # from O'Collins (Book Chapter) Table 20.6. infarct size in control cohorts -> However time point is not clear, should we rather use 
# values from Gerriet 2003 (Figure 4)? 
sd_L = 5
b_0 = 50 #previously this value was set to 50 - rationale? 
b_1 = c(0) # this values will be varied to simulate a causal treatment effect 
b_2 = c(3) # this values were set according after combining estimates from Gerriet 2003 and O'Collins (Book Chapter): 
# baseline stroke volume is 6h after MCAo, outcome measurement is 24h after MCAo
g_0 = 3
g_1 = c(-10,-60) #this values will be varied to simulate a treatment effect on welfare with different magnitudes  
g_2 = c(-9) #taken from Bederson et al. 1986 
sd_Y = 10 #previously this was set to 10 - rationale?
sd_W = 2
cutoff_W = c(0.1, 0.2, 0.3)
n = c(10, 20, 50) #total number of animals in the experiment

report <- expand_grid(prob_A, mean_L, sd_L, b_0, b_1, b_2, g_0, g_1, g_2, sd_Y, sd_W, cutoff_W, n)

naive_estimate <- matrix(NA_real_, ncol=B, nrow=nrow(report))
adjusted_mod_estimate <- matrix(NA_real_, ncol=B, nrow=nrow(report))
no_exclusion_estimate <- matrix(NA_real_, ncol=B, nrow=nrow(report))

set.seed(100)

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


sum(dat$L<dat$Y)
# assess bias
apply(naive_estimate, 1, mean)
apply(no_exclusion_estimate, 1, mean)
apply(adjusted_mod_estimate, 1, mean)

qt_95<-qt(0.975, 9)


#standard deviation
apply(naive_estimate, 1, sd)
apply(no_exclusion_estimate,1,sd)
apply(adjusted_mod_estimate, 1,sd)


df<-data.frame(mean_naive_estimate = round(apply(naive_estimate, 1, mean),2), 
               mean_no_exclusion_estimate = round(apply(no_exclusion_estimate, 1, mean),2),
               mean_adjusted_mod_estimate = round(apply(adjusted_mod_estimate, 1, mean),2), 
               sd_naive_estimate = round(apply(naive_estimate, 1, sd),2), 
               sd_no_exclusion_estimate = round(apply(no_exclusion_estimate,1, sd), 2),
               sd_adjusted_mod_estimate = round(apply(adjusted_mod_estimate,1, sd),2)
)

#calculating confidence intervals 

degrees_of_freedom<-n-2

df_n_g1_cutoff_W<-report%>%select("g_1", "n", "cutoff_W")

df<-cbind(df, df_n_g1_cutoff_W)

df<-df%>%
  rename(side_effect = g_1)%>%
  mutate(LC_naive_estimate = NA,
         LC_adjusted_mod_estimate = NA, 
         LC_no_exclusion_estimate = NA,
         UC_naive_estimate = NA,
         UC_adjusted_mod_estimate = NA, 
         UC_no_exclusion_estimate = NA)


for (i in 1:nrow(df)) {
  df$side_effect[i]<-ifelse(df$side_effect[i] == -10, "minor", "major")
  df$LC_naive_estimate[i]<-df$mean_naive_estimate[i]-qt(0.975, df$n[i])*df$sd_naive_estimate[i]
  df$LC_adjusted_mod_estimate[i]<-df$mean_adjusted_mod_estimate[i]-qt(0.975, df$n[i])*df$sd_adjusted_mod_estimate[i]
  df$LC_no_exclusion_estimate[i]<-df$mean_no_exclusion_estimate[i]-qt(0.975, df$n[i])*df$sd_no_exclusion_estimate[i]
  df$UC_naive_estimate[i]<-df$mean_naive_estimate[i]+qt(0.975, df$n[i])*df$sd_naive_estimate[i]
  df$UC_adjusted_mod_estimate[i]<-df$mean_adjusted_mod_estimate[i]+qt(0.975, df$n[i])*df$sd_adjusted_mod_estimate[i]
  df$UC_no_exclusion_estimate[i]<-df$mean_no_exclusion_estimate[i]+qt(0.975, df$n[i])*df$sd_no_exclusion_estimate[i]
}


df_estimate<-df%>%
  select(n,side_effect,cutoff_W, mean_naive_estimate:mean_adjusted_mod_estimate)%>%
  gather(key = model, value = estimate, mean_naive_estimate:mean_adjusted_mod_estimate)
