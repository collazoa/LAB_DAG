#L --> Y 
#from https://doi.org/10.1161/01.STR.31.8.1965 , Fig 3. 
# DIW measures in a time series 
# I took values for the 30min occlusion group 

setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/LAB_DAG/")
DIW<-read.csv(file = "DIW_time_series_Neumann_Haefelin.csv", sep = ";", header = TRUE)

for (i in 1:nrow(DIW)) {
  DIW$normalized_DWI<-as.numeric(DIW$normalized_DWI)
  DIW$rel_change[i]<-DIW$normalized_DWI[i]/DIW$normalized_DWI[2]
}
