#GLM to test which variables are the most important shaping the indices#
#Marina Scarpelli#
#07.01.2020#

library(tidyverse)
library(ggplot2)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

#Reads the data
df_indices <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"))

df_vegdata <- read.csv(getDataPath("Fieldwork_Bowra", "27.08.2019_Data.csv"))

#Joining the two datasets
df_IndicesAndVeg <- merge(x = df_indices, y = df_vegdata, by.x = "PointData", by.y = "Point")

#Treating the HOBOs data

#Putting the indices as a variable
df_wider <- gather(df_indices, c(BackgroundNoise, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfVarianceSpectrum, ClusterCount, Ndsi), key = "Index", value = "value")


