library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("SERF_AI_PreProcessed", "20.07.2020_151617indices.csv")) %>% 
  select(., BackgroundNoise, Snr, Activity, AvgSnrOfActiveFrames, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, EntropyOfCoVSpectrum, ClusterCount, Ndsi, SptDensity, FileName, location, date, year, month, day, time, file_id, ResultMinute, ResultStartSeconds)
write.csv(df, getDataPath("SERF_AI_PreProcessed", "20.07.2020_151617indices.csv"))

df <-  df %>% mutate_at(vars(1:17), scale)
write.csv(df, getDataPath("SERF_AI_PreProcessed", "20.07.2020_151617_scaledindices.csv"))
summary(df)

#correlation matrix between normalised indices
cor <- abs(cor(df[1:17], use = "complete.obs", method = "spearman")) %>% 
  write.csv(getDataPath("20.07.2020_correlationmatrix.csv"))

#Removing the highly correlated ones
scaled_df <- select(df, -c(Snr, Activity, AvgSnrOfActiveFrames, EventsPerSecond, SptDensity, MidFreqCover, EntropyOfVarianceSpectrum))

#Checking if all high correlations were removed
cor <- abs(cor(scaled_df[1:10], use = "complete.obs", method = "pearson")) %>% 
  write.csv(getDataPath("21.07.2020_correlationmatrix1.csv"))

write.csv(scaled_df, getDataPath("SERF_AI_PreProcessed", "21.07.2020_151617FinalIndices.csv"))
