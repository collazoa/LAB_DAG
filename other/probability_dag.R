library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggbeeswarm)



dat <- data.frame(E = rep(0:1,0.5*100, each=1),
                  L = rnorm(n=10, mean= 25, sd=5)) 

for (i in 1:nrow(dat)){
dat$L_cat <- ifelse(dat$L>quantile(dat$L, prob = 0.7), "high", "low")
dat$W <- ifelse(dat$L_cat == "high" & dat$E == 1, rbinom(1,1,0.9), rbinom(1,1,0.1))
dat$Y <- 50 + 0*dat$E + 4*dat$L
}

ggplot(dat) + geom_boxplot(aes(y = L, x = factor(W))

grid.arrange(                           
ggplot() + 
  geom_boxplot(aes(y = Y, x = factor(E)), data = dat[dat$W == 0,]), 
ggplot() + 
  geom_boxplot(aes(y = Y, x = factor(E)), data = dat[dat$W == 1,]) 
)



dat[dat$W ==1, ]
