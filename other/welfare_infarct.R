library(rstanarm)

data_frame<-data.frame(w = c(1:3), is<-c(10, 19, 28))
data<-stan_glm(is ~ w, data = data_frame) #estimate for w -> is : 9 
sims<-as.matrix(data)
ci<-quantile(sims[,2], c(0.025,0.975))
ci_80<-quantile(sims[,2], c(0.1,0.9))
#95% confidence interval: CI[5.4, 12.2]
#80% confidence interval: CI[7.6, 10.1]
