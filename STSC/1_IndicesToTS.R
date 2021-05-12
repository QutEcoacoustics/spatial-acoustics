library(tidyverse)

rm(list = ls())



getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

####Building TS - First create and unique identifier to each minute using the file name - which ideally has date and time embedded - and then scale the indices values - if necessary adjust the columns in line 15 - scale only columns with indices.

indices <- read.csv(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices", "Bowraaug_indices_complete.csv")) %>% 
  mutate(., FID = paste(FileName, ResultMinute, sep = "_")) %>% 
  mutate_at(vars(2:16), scale)

points <- as.list(levels(indices$point))

#Each index is an individual time series. To make it easier, we first assign index name, abbreviation and the location to create the time series for each location and index.

index_name <- "AcousticComplexity"
index_abb <- "ACI"
Location <- "Bowra"


for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", "Test", paste(p, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

index_name <- "EventsPerSecond"
index_abb <- "EVN"
Location <- "Bowra"


for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", "Test", paste(p, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

index_name <- "TemporalEntropy"
index_abb <- "ENT"
Location <- "Bowra"


for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", "Test", paste(p, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

#This one create one file with all the 3 indices + all variables needed for subsequence analysis.

for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., AcousticComplexity, EventsPerSecond, TemporalEntropy, FileName, filepath, point, date, time, ResultMinute, FID) %>% 
    write.csv(., getDataPath("STSC", "Test", paste(p, Location, "aug.csv", sep = "")))
}


#After this step you should go to powershell and run the HIME algorithm to each time series
