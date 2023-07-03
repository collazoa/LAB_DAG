source("./manuscript_figures_code/load_packages.R")

g_0 = 0
g_1 = c(-1, -3, -6) # strength if side effects A -> W 
g_2 = -1            # L -> W 
sd1 = sd2 = 5       # sd of initial infarct size 
mean_L = 25         # mean L value
cutoff_W = c(0.2, 0.3, 0.5)   #attrition frequencies 


report2 <- expand.grid(g_0 = g_0, 
                       g_1 = g_1, 
                       g_2 = g_2, 
                       sd1 = sd1, 
                       sd2 = sd2, 
                       mean_L = mean_L, 
                       cutoff_W = cutoff_W)
report2$mean1 <- NA
report2$mean2 <- NA
report2$quant_select <- NA
report2$prob_treated <- NA 
report2$prob_untreated <- NA
report2$prob_treated_r <- NA
report2$prob_untreated_r <- NA

# METHOD 1

for (i in 1:nrow(report2)){
report2$mean1[i] = report2$g_0[i] + report2$g_1[i] + report2$g_2[i]*report2$mean_L[i] 
#mean welfare score in the treated
report2$mean2[i] = report2$g_0[i] + report2$g_2[i]*report2$mean_L[i] 
#mean welfare score in the non-treated

report2$quant_select[i] <- qmixnorm(p = report2$cutoff_W[i], mean = c(report2$mean1[i], report2$mean2[i]), sd = c(report2$sd1[i], report2$sd2[i]), pro = c(0.5, 0.5)) 
# thresd on the welfare score that leaves the specified cutoff_W % of animals out
report2$prob_treated[i] <- pnorm(report2$quant_select[i], report2$mean1[i], sd = report2$sd1[i]) 
# probability of being out if you are in the treated group
report2$prob_untreated[i] <- pnorm(report2$quant_select[i], report2$mean2[i], sd = report2$sd2[i]) 
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
Threshold = quantile(c(Welfare_non_treated, Welfare_treated), probs = report2$cutoff_W[i])
report2$check_prob_out_treated[i] = mean(Welfare_treated < Threshold)
report2$check_prob_out_non_treated[i] = mean(Welfare_non_treated < Threshold)
}


report2
kable(report2)%>%kable_classic()

report3 <- select(report2, c("g_1", "cutoff_W", "prob_treated", "prob_untreated"))

report3 <- data.frame(report3[,1:2], round(report3[,3:4],2) * 100)

report3$cutoff_W <- factor(report3$cutoff_W, levels = c(0.2,0.3,0.5),
                           labels = c("20", "30", "50"))

report3$g_1 <- factor(report3$g_1,levels = c(-6, -3, -1), 
                             labels = c("major", "moderate", "minor"))

report3 <- 
  report3 %>%
  arrange(desc(g_1))

colnames(report3) <- c("side-effects", 
                       "total attrition rates (%)", 
                       "attrition rate in treatment group (%)", 
                       "attrition rate in control group (%)")

kable(report3, 
      caption = "Table S2: Attrition rates stratified by intervention group")%>%
  kable_classic() 


