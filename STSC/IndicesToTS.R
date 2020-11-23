library(tidyverse)

rm(list = ls())

####Building TS

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

indices <- read.csv(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "indices_all_AM.csv")) %>% 
  select(., 5:40) %>% 
  #separate(., col = FileName, into = c("location", "rec", "other"), sep = "-", remove = F) %>% 
  #separate(., col = other, into = c("point", "date", "time"), sep = "_", remove = T) %>%
  #separate(df, col = date, into = c("year", "monthday"), sep = 4, remove = F) %>% 
  #separate(., col = monthday, into = c("month", "day"), sep = 2, remove = T) %>% 
  mutate(., FID = paste(FileName, ResultStartSeconds, sep = "_")) %>% 
  mutate_at(vars(5:22), scale)

points <- as.list(unique(indices$Transectpoint))

index_name <- "EventsPerSecond"
index_abb <- "EVN"
Location <- "Bowra"
# target_month <- 03
# target_year <- 2015


for (point in points) {
  indices %>% 
    filter(., Transectpoint == point) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name)) %>% 
    write.table(., getDataPath("STSC", paste(point, index_name, Location, "aug.txt", sep = "")), row.names = F, col.names = F)
  
}

for (point in points) {
  indices %>% 
    filter(., Transectpoint == point) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., AcousticComplexity, EventsPerSecond, TemporalEntropy, FileName, originalfile, Location, Transectpoint, rec, date, time, ResultStartSeconds, ResultMinute, FID) %>% 
    write.csv(., getDataPath("STSC", "Results", "Bow_aug", paste(point, Location, "aug.csv", sep = "")))
}

