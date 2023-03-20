install.packages("KScorrect")
library(KScorrect)
library(tidyverse)
library(kableExtra)

gamma0 = 3
gamma1 = c(-10, -30, -60)
gamma2 = -10
sd1 = sd2 = 10
L = 25 # mean L value
attr_rate = c(0.1, 0.2, 0.3)


report2 <- expand.grid(gamma0 = gamma0, 
                       gamma1 = gamma1, 
                       gamma2 = gamma2, 
                       sd1 = sd1, 
                       sd2 = sd2, 
                       L = L, 
                       attr_rate = attr_rate)
report2$mean1 <- NA
report2$mean2 <- NA
report2$quant_select <- NA
report2$prob_treated <- NA 
report2$prob_untreated <- NA
report2$prob_treated_r <- NA
report2$prob_untreated_r <- NA

# METHOD 1

for (i in 1:nrow(report2)){
  report2$mean1[i] = report2$gamma0[i] + report2$gamma1[i] + report2$gamma2[i]*report2$L[i] 
  #mean welfare score in the treated
  report2$mean2[i] = report2$gamma0[i] + report2$gamma2[i]*report2$L[i] 
  #mean welfare score in the non-treated
  
  report2$quant_select[i] <- qmixnorm(p = report2$attr_rate[i], mean = c(report2$mean1[i], report2$mean2[i]), sd = c(report2$sd1[i], report2$sd2[i]), pro = c(0.5, 0.5)) 
  # threshold on the welfare score that leaves the specified attr_rate % of animals out
  report2$prob_treated[i] <- pnorm(report2$quant_select[1], report2$mean1[i], sd = report2$sd1[i]) 
  # probability of being out if you are in the treated group
  report2$prob_untreated[i] <- pnorm(report2$quant_select[1], report2$mean2[i], sd = report2$sd2[i]) 
  # probability of being out if you are in the non-treated group
  report2$prob_treated_r[i] <- round(report2$prob_treated[i],2) 
  report2$prob_untreated_r[i] <- round(report2$prob_untreated[i], 2) 
}

# CHECK in a simulation that the proportions match
n = 1000000

report2$check_prob_out_treated <- NA 
report2$check_prob_out_non_treated <- NA

for (i in 1:nrow(report2)) {
  Welfare_treated = rnorm(n, report2$mean1[i], report2$sd1[i])
  Welfare_non_treated = rnorm(n, report2$mean2[i], report2$sd2[i])
  Threshold = quantile(c(Welfare_non_treated, Welfare_treated), probs = report2$attr_rate[i])
  report2$check_prob_out_treated[i] = mean(Welfare_treated < Threshold)
  report2$check_prob_out_non_treated[i] = mean(Welfare_non_treated < Threshold)
}

kable(report2)%>%kable_classic()