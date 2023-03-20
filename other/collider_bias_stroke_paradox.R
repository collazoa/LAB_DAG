library(tidyverse)
set.seed(42)
N = 10000
# X and D cause Y
# A is exposure, binary 
# S1 is welfare score, continous, but defined cut-off point 
# L is unmeasured baseline infarct size 
# A -> S1 <- L
A = rbinom(N, 1, p = 0.5)
L = abs(rnorm(N, 2.5, 1))
S1 <-vector("numeric", length = N)
Y = -1 + 1.3^L # assuming a logarithmic relationship between L (baseline infarct) and Y (measured endpoint infarct size)
#it is assumed that the infarct size cannot increase for any L, so Y < L 

for (i in 1:N){
  if (A[i] == 1 & L[i]< 3.5) {
    S1[i]<- rbinom(1,1, prob = 0.5)
  }
  if (A[i] == 1 & L[i] >=3.5){
    S1[i]<-rbinom(1,1,prob = 1)
  }
  if (L[i] >= 3.5 & A[i]==0) {
    S1[i]<-rbinom(1,1, prob = 0.2)
  } 
  if(L[i]< 3.5 & A[i] ==0){
    S1[i]<-rbinom(1,1,prob = 0.05)
  }
}


df = data.frame(
  A=A,
  L=L,
  S1=S1, 
  Y = Y
)

#visualizing the relationship between treatment assignment A and infarct size L 
#no observable difference 
boxplot(df$L ~ df$A)


#creating new variable "censored" when S1 exceeds certain treshhold (S1 >=6)
df<-df%>%
  mutate(censored = ifelse( S1 == 1 , TRUE , FALSE ))

df %>% group_by(A) %>% summarize(n = sum(censored))
df %>% group_by(A) %>% summarize(meanL = mean(L))

df %>% group_by(censored, A) %>% summarize(meanL = mean(L))



df_uncensored<-df %>% filter(censored == 0)

ggplot(df_uncensored) + geom_boxplot(aes(y = L, group = A))


df_censored<-df %>% filter(censored == 1)

ggplot(df_censored) + geom_boxplot(aes(y = L, group = A))


#t-testing L on A in the whole sample: L does not differ significantly between treatment and control group 
t.test(df$L ~df$A)
#p-value = 0.4664


#t-testing L on A in the censored sample:  L does differ significantly between treatment and control group
#the control group shows higher values for L 
t.test(df_censored$L ~ df_censored$A)
#p-value = 0.05427


#t-testing L on A in the uncensored sample: L does differ significantly between treatment and control group 
#the control group shows higher values for L 
t.test(df_uncensored$L ~ df_uncensored$A)


#introducing Y 

df %>% group_by(A) %>% summarize(meanY = mean(Y))
df %>% group_by(censored, A) %>% summarize(meanY = mean(Y))

df_uncensored<-df %>% filter(censored == 0)
ggplot(df_uncensored) + geom_boxplot(aes(y = Y, group = A))


df_censored<-df %>% filter(censored == 1)
ggplot(df_censored) + geom_boxplot(aes(y = Y, group = A))


