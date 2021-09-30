library(tidyverse)

B=10000

# A bernuoulli prob_A
# L is normal with mean_L and sd_L
# Y = b_0 + b_1*A + b_2*L + rnorm(0,sd_Y)
# W = g_0 + g_1*A + g_2*L + rnorm(0, sd_W)

prob_A = 0.5
mean_L = 150
sd_L = 40
b_0 = 50
b_1 = 0
b_2 = 0.8
g_0 = 3
g_1 = c(-5,-3,-1, -10)
g_2 = c(-0.1,-0.3,-0.5)
sd_Y = 50
sd_W = 2
cutoff_W = c(0.1,0.2)
n = 500

report <- expand_grid(prob_A, mean_L, sd_L, b_0, b_1, b_2, g_0, g_1, g_2, sd_Y, sd_W, cutoff_W, n, bias=NA_real_, check=NA_real_, estimate=NA_real_)

# here starts the counting of the row
for (i in 1:nrow(report)) {

estimate <- rep(NA_real_, B)
bias <- rep(NA_real_, B)
check <- rep(NA_real_, B)

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
bias[b] = estimate[b] - report$b_1[i]
check[b] = mean(dat$Y[dat$A==1]) - mean(dat$Y[dat$A==0])
}
report$estimate[i] = mean(estimate)
report$bias[i] = mean(bias)
report$check[i] = mean(check)
}
