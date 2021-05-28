# Load libraries
library(tidyverse)
library(kableExtra)
library(tidymodels)
library(viridisLite)
library(GGally)
library(dagitty)
library(ggdag)
library(visreg)
library(styler)
library(cowplot)


# assign DAG object

# Collider bias example 
k<- dagitty( "dag {
  A -> S1 <- L -> Y
  A -> Y
  S1-> S2
  A [exposure]
  Y [outcome]
  }")

#change coordinates for plot 
coordinates(k) <-  list(
  x=c(A=1, Y=5, L=1, S1 = 1.5, S2 = 2.5),
  y=c(A=5, Y = 5, L = 1, S1 = 3, S2 = 3) )
plot(k)
# tidy the dag object and suppply to ggplot
tidy_dagitty(k)%>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(color = "gray",
                alpha = 0.8) +
  geom_dag_edges(
    edge_colour = "black",
    edge_width = .8
  ) +
  geom_dag_text(color = "white")+
  geom_dag_text() +
  theme_dag()

#create a table with explanation for the variables 
legend<-data.frame(variables = c("A", "S1", "S2", "L1", "Y"), legend = c("Thrombolytic", "Welfare Score", "Censoring", "Unobserved baseline infarct size", "Observed infarct size"))
legend%>%kbl%>%kable_classic(full_width = F)

legend1<-data.frame(variables = c("A", "S1", "S2", "L1", "Y"), legend = c("Neuroprotective agent", "Welfare Score", "Censoring", "Infection", "Neurological score"))
legend1%>%kbl%>%kable_classic(full_width = F)

#measurement bias example 
m <- dagitty( "dag {
  A -> L1 -> L2-> Y1 <- Y
  A -> Y
  A [exposure]
  Y [outcome]
  }")

coordinates(m) <-  list(
  x=c(A=1, Y=5, L1=3, L2 = 5, Y1 = 5),
  y=c(A=5, Y = 5, L1 = 3, L2= 1, Y1 = 3) )
plot(m)

tidy_dagitty(m)%>%
  mutate(y = c(1, 1, 3, 5, 1,3), 
         yend = c(3,1,5,3,3, NA))%>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(color = "gray",
                alpha = 0.8) +
  geom_dag_edges(
    edge_colour = "black",
    edge_width = .8
  ) +
  geom_dag_text(color = "white")+
  geom_dag_text() +
  theme_dag()

legend2<-data.frame(variables = c("A", "L1", "L2", "Y1", "Y"), legend = c("Anxiolytic", "Motor dysfunction", "Mobility", "Exploratory behaviour", "Anxiety"))
legend2%>%kbl%>%kable_classic(full_width = F)



#confounder 
con<- dagitty( "dag {
  A <-L1 -> L2 -> Y
  A <-L1 -> L3 -> Y
  A -> Y
  A [exposure]
  Y [outcome]
  }")

coordinates(con) <-  list(
  x=c(A=3, Y=5, L1=1, L2 = 2, L3 = 2),
  y=c(A=5, Y = 5, L1 = 4.5, L2= 4.5, L3 = 4) )
plot(con)


tidy_dagitty(con)%>%
  mutate(
    y = c(1,3,3,3,3,4,1 ),
    yend= c(1,1,3,4,1,1,NA )
  ) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(color = "gray",
                alpha = 0.8) +
  geom_dag_edges(
    edge_colour = "black",
    edge_width = .8
  ) +
  geom_dag_text(color = "white")+
  geom_dag_text() +
  theme_dag()


legend_con<-data.frame(variables = c("A", "L1", "L2", "L3", "Y"), legend = c("gene knock-out", "knockout line breeding", "dam caring behaviour", "sensorial dysfunction", "behavioural outcome"))
legend_con%>%kbl%>%kable_classic(full_width = F)
