library(tidyverse)
library(ggplot2)
library(stringr)
library(purrr)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

directory <- (getDataPath("SERF_AI"))

output_dir <- (getDataPath("SERF_AI_PreProcessed"))

files <- list.files(directory, pattern = ".Indices.csv", full.names = T, recursive = T)

name <- basename(files)
name <- gsub(pattern = "__Towsey.Acoustic.Indices.csv", replacement = "", x = name)

files <- as.list(files)
df <- lapply(files, read.csv) 
df<-do.call(rbind, df)
df <- select(df, BackgroundNoise, Snr, Activity, AvgSnrOfActiveFrames, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, EntropyOfCoVSpectrum, ClusterCount, Ndsi, SptDensity, FileName, ResultMinute, ResultStartSeconds)
df <- separate(df, col = FileName, into = c("location", "date", "time", "file_id"), sep = "_", remove = F)
  write.csv(df, getDataPath("SERF_AI_PreProcessed", "20.07.2020_151617indices.csv"))
  

df <- separate(df, col = date, into = c("year", "monthday"), by = ,4, remove = F) %>% 
separate(., col = monthday, into = c("month", "day"), by = ,2, remove = T)
write.csv(df, getDataPath("SERF_AI_PreProcessed", "20.07.2020_151617indices.csv"))

rm(files)
