library(tidyverse)
library(purrr)

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey",  ...))
}

files <- list.files(path = getDataPath("RemoteSensing", "2_SentinelHub_Landsat_SAVIExtracted"), pattern = ".csv", recursive = T, full.names = T)
name <- basename(files)
name <- gsub(pattern = ".csv", replacement = "", x = name)

df <- list()

for (file in files) {
  read.csv(file) %>% 
  select(., "SAVI_AVG", "ID", "DATE") %>% 
  assign(paste("df", basename(files), sep = ""), file)
} 


%>% 
  do.call(rbind, files) -> df
