# data transformation for visualization 

m1_tibble <- as_tibble(m1) %>% mutate(model = "m1", 
                                      index = 1:27)
m2_tibble <- as_tibble(m2) %>% mutate(model = "m2", 
                                      index = 1:27)
m3_tibble <- as_tibble(m3) %>% mutate(model = "m3", 
                                      index = 1:27)

df <- rbind(m1_tibble, m2_tibble, m3_tibble)

for (i in 1:10000) {
names[i] <- c(paste("estimate_",i, sep = ""))
}
colnames(df)<-c(names, "model", "index") 



df2 <- gather(df, 
              key  = sim_number, 
              value = effect_estimate, 
              estimate_1:estimate_10000)

df3 <- report %>% 
            mutate(index = 1:nrow(report))%>%
            select(c(index,n, cutoff_W, g_1))
  
df3$g_1 <- factor(df3$g_1,levels = c(-60, -30, -10), 
                    labels = c("major", "moderate", "minor"))
df3$cutoff_W <- factor(df3$cutoff_W, levels = c(0.2,0.3,0.5),
                       labels = c("20% attrition", "30% attrition", "50% attrition"))




df4 <- inner_join(df2, df3)

#check for correct number of rows per model and scenario (index)
#df3 %>%group_by(index, model)%>%
#          summarize(n = n())

#sum(is.na(df4$effect_estimate)) 


df5 <- df4 %>%
        group_by(index, model)%>%
        summarize(mean = mean(effect_estimate, na.rm = TRUE), 
                  quan25 = quantile(effect_estimate, 0.25, na.rm = TRUE), 
                  quan75 = quantile(effect_estimate, 0.75, na.rm = TRUE))

df5 <- inner_join(df5, df3)


fig4<- ggplot(na.omit(df4))+
  geom_point(aes(x = g_1, y = mean(effect_estimate), color = model))+
  facet_grid(cols = vars(factor(n)), 
             rows = vars(factor(cutoff_W)))+
  scale_y_continuous(breaks = c(0, -5, -10, -15))+
  scale_color_manual(values = c(cols[1], cols[4], cols[8]), 
                     labels = c("Model 1", "Model 2", "Model 3"))+
  labs(title = "Model comparison: adjustment for initial infarct size mitigates collider stratification bias",
       y = expression("Y"^"E = 1"*"- Y"^"E = 0"), 
       x = "strength of negative side-effect of treatment on welfare")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank())
fig4

