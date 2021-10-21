library(tidyverse)
library(ggplot2)

B=1000

# A bernuoulli prob_A
# L is normal with mean_L and sd_L
# Y = b_0 + b_1*A + b_2*L + rnorm(0,sd_Y)
# W = g_0 + g_1*A + g_2*L + rnorm(0, sd_W)


#example of settings for coefficients 
#probably b_2 & g_2 can be fixed from literature 
prob_A = 0.5
mean_L = 150
sd_L = 40
b_0 = 50
b_1 = 0 # this values will be varied to simulate a causal treatment effect 
b_2 = 1.5 # should this value be larger than 1? -> initial increase of infarct size 
g_0 = 3
g_1 = c(-100,-50,-30) #this values will be varied to simulate a treatment effect on welfare with different magnitudes  
g_2 = c(-0.3) #do we need to vary this? -> identify from animal model studies 
sd_Y = 50
sd_W = 2
cutoff_W = c(0.1,0.2, 0.3)
n = 20

report <- expand_grid(prob_A, mean_L, sd_L, b_0, b_1, b_2, g_0, g_1, g_2, sd_Y, sd_W, cutoff_W, n, bias=NA_real_, check=NA_real_, estimate=NA_real_)

# here starts the counting of the row
for (i in 1:nrow(report)) {

estimate <- rep(NA_real_, B)
bias <- rep(NA_real_, B)
check <- rep(NA_real_, B)
estimate_perc <- rep(NA_real_, B)

# here starts the loop in b
for (b in 1:B) {

# create the dataset
dat <- data.frame(A = rep(0:1,report$prob_A[i]*report$n[i], each=1),
                  L = rnorm(n=report$n[i], mean=report$mean_L[i], sd=report$sd_L[i]))
dat$W <- report$g_0[i] + report$g_1[i]*dat$A + report$g_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_W[i])
dat$Y <- report$b_0[i] + report$b_1[i]*dat$A + report$b_2[i]*dat$L + rnorm(report$n[i], 0, report$sd_Y[i])
dat$W2 <- dat$W >= quantile(dat$W, probs=report$cutoff_W[i]) 
dat_s <- dat %>% filter(W2==TRUE)
# dat_s %>% group_by(A) %>% summarise(Y=mean(Y))
estimate[b] =  mean(dat_s$Y[dat_s$A==1]) - mean(dat_s$Y[dat_s$A==0])
estimate_perc[b] = (mean(dat_s$Y[dat_s$A ==1]) - mean(dat_s$Y[dat_s$A==0]))/mean(dat_s$Y[dat_s$A==0])
bias[b] = estimate[b] - report$b_1[i]
check[b] = mean(dat$Y[dat$A==1]) - mean(dat$Y[dat$A==0])
}
report$estimate[i] = mean(estimate)
report$bias[i] = mean(bias)
report$check[i] = mean(check)
report$estimate_perc[i] = mean(estimate_perc)
}


#separate displays of mean difference between treatment and intervention group for different strengths of A -> W (side-effects)

report_large<-report%>%filter(report$g_1 == -100)%>%gather(key = "estimate", value = "value", check:estimate)%>% select(-bias)
report_medium<-report%>%filter(report$g_1 == -50)%>%gather(key = "estimate", value = "value", check:estimate)%>% select(-bias)
report_small<-report%>%filter(report$g_1 == -30)%>%gather(key = "estimate", value = "value", check:estimate)%>% select(-bias)

report_long<-rbind(report_large, report_medium, report_small)
#ggplot(report_large)+ geom_point(mapping = aes(x = estimate, y = value, color = estimate)) + facet_grid(cols = vars(cutoff_W))
#ggplot(report_medium)+ geom_point(mapping = aes(x = estimate, y = value, color = estimate)) + facet_grid(cols = vars(cutoff_W))
#ggplot(report_small)+ geom_point(mapping = aes(x = estimate, y = value, color = estimate)) + facet_grid(cols = vars(cutoff_W))

#side_effects<-c("major", "moderate", "minor")
#names(side_effects)<-c(-100, -50, -30)
#attrition<-c("10% attrition", "20% attrition", "30% attrition")
#names(attrition)<-c(0.1,0.2,0.3)

report_long$g_1 <- factor(report_long$g_1,levels = c(-100, -50, -30), 
                  labels = c("major side-effects", "moderate side-effects", "minor side-effects"))
report_long$cutoff_W <- factor(report_long$cutoff_W, levels = c(0.1,0.2,0.3),
                  labels = c("10% attrition", "20% attrition", "30% attrition"))
report_long$estimate<-factor(report_long$estimate, levels = c("estimate", "check"), labels = c("censored data", "uncensored data"))

report_long$estimate_perc<-round(report_long$estimate_perc*100)

ggplot(report_long)+
  geom_point(aes(x = estimate, y = value, color = estimate))+
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")+
  facet_grid(cols = vars(factor(cutoff_W)), 
             rows = vars(factor(g_1)))+
  labs(title = "Comparison of effect estimate without and with collider stratification under no treatment effect",
       y = "estimated mean reduction in absolute infarct size between treatment and control group") + 
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "none")


report_perc<-report_long%>%filter(report_long$estimate =="censored data")
ggplot(report_perc)+
  geom_col(aes(x = cutoff_W, y = estimate_perc, fill = cutoff_W))+
  facet_grid(cols = vars(factor(g_1)))+
  labs(title = "Artefact relative infarct size reduction between treatment and control group",
       y = "estimated percent point difference between treatment and controll group under collider stratification") + 
  theme(panel.grid.major = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank())
