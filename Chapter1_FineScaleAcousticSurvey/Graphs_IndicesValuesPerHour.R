#Plotting indices per hour#
#Marina Scarpelli#
#07.01.2020#

library(tidyverse)
library(ggplot2)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

#Reads the data
df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"))

#Putting the indices as a variable
df_wider <- gather(df, c(BackgroundNoise, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfVarianceSpectrum, ClusterCount, Ndsi), key = "Index", value = "value")



#Plotting all the sites together

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
         geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "Index Scaled Values") +
  ggtitle("Scaled Indices Values by time of the day (in hours)") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "07.01.2020_indicesbytimeofday.jpg"))
  
  

#Plotting per hour per site

indicesbytimeofday +
  scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 060000, 120000, 180000, 240000)) +
  facet_wrap(PointData ~ .) +
ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "07.01.2020_indicesbytimeofdaypersite.jpg"))

