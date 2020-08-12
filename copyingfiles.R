library(tidyverse)

directory <- setwd("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment/RemoteSensing_SERF_zip")
output <- ("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment/SERF_RemoteSensing_NDWI")

files <- list.files(directory, pattern = "*NDWI.tiff", recursive = T, full.names = T)
file.copy(files, to = output, overwrite = F, recursive = F, copy.mode = T, copy.date = T)

