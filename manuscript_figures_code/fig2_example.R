library(tidyverse)
library(gridExtra)
library(ggbeeswarm)


# settings for styling visualization
cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(face="bold"))


#######################################
# function to simulate collider bias   
#######################################

sim.dat <- function(N, b_0, b_1, b_2, g_0, g_1, g_2,
                    sd_w, sd_y, cutoff_W){
  X <- rep(0:1, each=N)
  
  # causal model
  L <- rnorm(n = 2*N, mean = 25, sd = 5)
  W <- g_0 + g_1 * X + g_2*L + rnorm(N, 0, sd_w)
  Y <- b_0  + b_1 * X + b_2 * L+ rnorm(N, 0, sd_y)
  S <- W >= quantile(W, probs = cutoff_W)
  
  return(data.frame(X, L, Y, W, S))    
}


############################################
# creating a illustrative example dataset  
# for Fig 2 and Fig 3                       
############################################

set.seed(290) # reproduces a specific random sample  

#########################################################
# d is the complete data set (corresponding to Model 1)
# d_s is the censored data set (corresponding to Model 2)
# attrition is moderate (30%), negative side-effects on 
# animal welfare are moderate 
# total sample size chosen is n = 20 


d <- sim.dat(N=10, b_0=50, b_1= 0 , b_2 = 4,
             g_0= 3, g_1= -3, g_2 = -1, 
             sd_w= 2, sd_y= 10, cutoff_W = 0.3)


d_s<-d %>% filter(S == 1)

#########################################################


#########################################################
#mutating data to prepare for visualization 
d <- d %>%
  mutate(cols=factor(d$X, labels = c(cols[1],cols[4])),
         pch=recode(X, `0`=1, `1`=17),
         S1 =factor(d$S, labels = c(0,1)),
         bg = recode(S1, `0`= 0.3, `1`=1 ),
         X1=factor(recode(X, `0`="Control", `1`="Drug")))

d_s<-d_s%>%
  mutate(cols=factor(d_s$X, labels = c(cols[1],cols[4])),
         pch=recode(X, `0`=1, `1`=17),
         X1=factor(recode(X, `0`="Control", `1`="Drug")))

d_summary<-d%>%group_by(X)%>%
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

#######################################################



######################################################
# visualization for Figure 2 


fig2<-grid.arrange(
  ggplot()+
    geom_quasirandom(data = d, aes(y = L, x = X1), shape=21, 
                     color="darkblue", fill = d$cols, alpha = d$bg, 
                     size = 3, width=0.1)+
    geom_segment(data = d_summary, 
                 aes(x = x, y = mean_L, yend = mean_L, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 0.3)+
    geom_segment(data = d_s_summary, 
                 aes(x = x, y = mean_L, yend = mean_L, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 1)+
    ylab(expression("Initial infarct size (mm" ^3*")")) + 
    xlab("") + 
    ylim(15, 40) + 
    ggtitle("A")+
    letter, 
  
  
  ggplot() +
    geom_quasirandom(data = d, aes(y=Y, x=X1, group = X1), 
                     shape=21, color="darkblue", fill = d$cols, 
                     alpha = d$bg, size = 3, width = 0.1) +
    geom_segment(data = d_summary, 
                 aes(x = x, y = mean_Y, yend = mean_Y, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 0.3)+
    geom_segment(data = d_s_summary, 
                 aes(x = x, y = mean_Y, yend = mean_Y, xend = xend),
                 color = d_summary$cols, size = 1, alpha = 1)+
    ylab(expression("Final infarct size (mm"^3*")")) + 
    xlab("") + 
    ylim(100, 200) + 
    ggtitle("B")+
    letter,
  
  nrow=1, ncol=2)

fig2 

###########################################################





