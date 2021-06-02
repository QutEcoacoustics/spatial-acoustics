library(tidyverse)
library(seewave)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
sensor_point <- "WA01"
# location2 <- "WA011"
chapter <- "Chapter1_FineScaleAcousticSurvey"

field_data <- read.csv(getDataPath("Fieldwork_Bowra", "27.08.2019_Data.csv")) %>% 
  select(Point, location)


info <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  merge(., field_data, by.x = "point", by.y = "Point", all.x = T) %>% 
  mutate(folder = paste(location, "_", "REC", RECORDER, sep = ""))


soundfiles <- for (row in 1:length(info)) {
  
  wave <- readw
  
}
  