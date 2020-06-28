library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("SERF_AI_PreProcessed", "26.06.2020_131415indices.csv")) %>% 
  select(., BackgroundNoise, Snr, Activity, AvgSnrOfActiveFrames, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, EntropyOfCoVSpectrum, ClusterCount, Ndsi, SptDensity, FileName, location, date, year, month, day, time, file_id, ResultMinute, ResultStartSeconds)
write.csv(df, getDataPath("SERF_AI_PreProcessed", "26.06.2020_131415indices.csv"))

df1 <- as.data.frame(scale(df[1:17]))
scaled_df <- cbind(df1, df[18:27])
write.csv(scaled_df, getDataPath("SERF_AI_PreProcessed", "26.06.2020_131415ScaledIndices.csv"))
summary(scaled_df)
rm(df, df1)

#correlation matrix between normalised indices
cor <- abs(cor(scaled_df[1:17], use = "complete.obs", method = "pearson")) %>% 
  write.csv(getDataPath("26.06.2020_correlationmatrix.csv"))

#Removing the highly correlated ones
scaled_df <- select(scaled_df, -c(Snr, Activity, TemporalEntropy, MidFreqCover, EntropyOfVarianceSpectrum))

#Checking if all high correlations were removed
cor <- abs(cor(scaled_df[1:12], use = "complete.obs", method = "pearson")) %>% 
  write.csv(getDataPath("26.06.2020_correlationmatrix1.csv"))

write.csv(scaled_df, getDataPath("SERF_AI_PreProcessed", "26.06.2020_131415FinalIndices.csv"))
