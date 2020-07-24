#Plotting indices per hour#
#Marina Scarpelli#

library(tidyverse)
library(ggplot2)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

#Reads the data
df <- read.csv(getDataPath("SERF_AI_PreProcessed", "21.07.2020_151617FinalIndices.csv"))

#Putting the indices as a variable
df_wider <- gather(df, c(BackgroundNoise, TemporalEntropy, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfCoVSpectrum, ClusterCount, Ndsi), key = "Index", value = "value")

summary(df_wider)

#Plotting all the sites together
df_wider %>% filter(., Index == "Ndsi") %>% 
ggplot(., aes(x = day, y = value, colour = as.factor(year))) +
  geom_smooth() +
  theme_minimal() +
  scale_y_continuous(name = "Index Scaled Values") +
  ggtitle("Scaled NDSI per month") +
  facet_wrap(~ month) +
  ggsave(getDataPath("21.07.2020_3yrNDSI.jpg"))
