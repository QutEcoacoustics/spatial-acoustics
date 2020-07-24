library(tidyverse)

directory <- setwd("D:/Samford Data/Fluxtower/SERF_fluxtower physico-chemical_samford_data_2010-2018/2017")
output <- ("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment/WeatherData/Fluxtower")

files <- list.files(directory, pattern = "*FT_slow_met_2017*", recursive = T, full.names = T)
file.copy(files, to = output, overwrite = F, recursive = F, copy.mode = T, copy.date = T)

