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

point <- "WB56"

#Putting the indices as a variable
pivot_longer(df, c(BackgroundNoise, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfVarianceSpectrum, ClusterCount, Ndsi), values_to = "values", names_to = "index") %>% 
  filter(PointData == point) %>% 
  ggplot(., aes(x = index, y = values)) +
  geom_violin() +
  theme_minimal() +
  scale_y_continuous(name = "Index Scaled Values") +
  ggtitle(paste(point, "indices", sep = " ")) +
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", paste(point, "violin_indices.jpg")))
