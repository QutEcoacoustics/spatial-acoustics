library(tidyverse)

rm(list = ls())



getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

####Building TS

indices <- read.csv(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices", "Bowraaug_indices_complete.csv")) %>% 
  #select(., 5:40) %>% 
  #separate(., col = FileName, into = c("location", "rec", "other"), sep = "-", remove = F) %>% 
  #separate(., col = other, into = c("point", "date", "time"), sep = "_", remove = T) %>%
  #separate(df, col = date, into = c("year", "monthday"), sep = 4, remove = F) %>% 
  #separate(., col = monthday, into = c("month", "day"), sep = 2, remove = T) %>% 
  mutate(., FID = paste(FileName, ResultMinute, sep = "_")) %>% 
  mutate_at(vars(1:15), scale)

points <- as.list(levels(indices$point))

index_name <- "AcousticComplexity"
index_abb <- "ACI"
Location <- "Bowra"
# target_month <- 03
# target_year <- 2015


for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", paste(p, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

index_name <- "EventsPerSecond"
index_abb <- "EVN"
Location <- "Bowra"
# target_month <- 03
# target_year <- 2015


for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", paste(p, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

index_name <- "TemporalEntropy"
index_abb <- "ENT"
Location <- "Bowra"
# target_month <- 03
# target_year <- 2015


for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", paste(p, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

for (p in points) {
  indices %>% 
    filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., AcousticComplexity, EventsPerSecond, TemporalEntropy, FileName, filepath, point, date, time, ResultMinute, FID) %>% 
    write.csv(., getDataPath("STSC", "Results", "Bowraaug", p, paste(p, Location, "aug.csv", sep = "")))
}

