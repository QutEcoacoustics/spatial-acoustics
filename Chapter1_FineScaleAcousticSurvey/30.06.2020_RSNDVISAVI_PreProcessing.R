library(tidyverse)
library(purrr)

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey",  ...))
  
}


getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey",  ...))
  
}

files <- list.files(path = getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted"), pattern = ".csv", recursive = T, full.names = T)

df <- lapply(files, read.csv) %>% 
  map_df(., select, c("NDVI_AVERAGE", "ID", "DATE")) %>% 
  write.csv(., getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted", "01.07.2020_AVGNDVI.csv"))


