library(tidyverse)
library(gridExtra)
library(ggbeeswarm)
library(mediation)

cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(face="bold"))


sim.dat <- function(N, beta0, beta1, beta2, theta0, theta1, alpha,
                    sigma.w, sigma.y, cutoff){

  
  ## groups
  X <- rep(0:1, each=N)
  
  ## causal model
  L <- rnorm(n = 2*N, mean = 25, sd = 10)
  W <- beta0 + beta1 * X + beta2*L + rnorm(N, 0, sigma.w)
  Y <- theta0 + theta1 * L + alpha * X + rnorm(N, 0, sigma.y)
  S <- W <= quantile(W, probs = cutoff)
  
  return(data.frame(X, L, Y, W, S))    
}

#creating dataset 
set.seed(10)
d <- sim.dat(N=20, beta0=3, beta1=-40, beta2 = -10,
             theta0= 50, theta1=10, alpha=0, 
             sigma.w= 20, sigma.y= 20, cutoff = 0.3)


#biased (censored dataset)

d_s<-d %>% filter(S == 0)

#mutating data to prepare for visualization 

d <- d %>%
  mutate(cols=recode(X, `0`=cols[1], `1`=cols[4]),
         pch=recode(X, `0`=1, `1`=17),
         S1 =factor(d$S, labels = c(0,1)),
         bg = recode(S1, `0`= 1, `1`=0.3 ),
         X1=factor(recode(X, `0`="Control", `1`="Drug")))


d_summary<-d%>%group_by(X1)%>%
  summarize(mean_L = mean(L),
            mean_Y = mean(Y),
            sd_L = sd(L), 
            sd_Y = sd(Y))%>%
  mutate(cols = c(cols[1],cols[4]), 
         xend = c(1.3, 2.3), 
         x = c(0.7, 1.7))



d_s_summary<-d_s%>%group_by(X)%>%
  summarize(mean_L = mean(L),
            mean_Y = mean(Y),
            sd_L = sd(L), 
            sd_Y = sd(Y))%>%
  mutate(cols = c(cols[1],cols[4]), 
         xend = c(1.3, 2.3), 
         x = c(0.7, 1.7))





#visualization plots 

f<-ggplot(d_summary, aes(x = X1, y = mean_Y, ymin = mean_Y - sd_L, ymax = mean_Y + sd_Y))
f + geom_pointrange(color = d_summary$cols)


grid.arrange(
  ggplot()+
    geom_quasirandom(data = d, aes(y = L, x = X1), shape=21, 
                     color="darkblue", fill = d$cols, alpha = d$bg, 
                     size = 3, width=0.1)+
    geom_segment(data = d_summary, aes(x = x, y = mean_L, yend = mean_L, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 0.3)+
    geom_segment(data = d_s_summary, aes(x = x, y = mean_L, yend = mean_L, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 1)+
    ylab("Initial infarct size (mm³)") + xlab("") + ylim(30, 100) + ggtitle("A")+
    letter, 
  
  
  ggplot() +
    geom_quasirandom(data = d, aes(y=Y, x=X1, group = X1), 
                     shape=21, color="darkblue", fill = d$cols, 
                     alpha = d$bg, size = 3, width=0.1) +
    geom_segment(data = d_summary, aes(x = x, y = mean_Y, yend = mean_Y, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 0.3)+
    geom_segment(data = d_s_summary, aes(x = x, y = mean_Y, yend = mean_Y, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 1)+
    ylab("Final infarct size (mm³)") + xlab("") + ylim(100, 400) + ggtitle("B")+
    letter,
  
  nrow=1, ncol=2)

