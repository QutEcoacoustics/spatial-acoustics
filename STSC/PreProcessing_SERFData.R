library(tidyverse)

rm(list = ls())

####Building TS

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

indices <- read.csv(getDataPath("Chapter2_SoundscapeTemporalAssessment", "SERF_AI_PreProcessed", "26.06.2020_131415indices.csv")) %>% 
  #separate(., col = FileName, into = c("location", "rec", "other"), sep = "-", remove = F) %>% 
  #separate(., col = other, into = c("point", "date", "time"), sep = "_", remove = T) %>%
  #separate(df, col = date, into = c("year", "monthday"), sep = 4, remove = F) %>% 
  #separate(., col = monthday, into = c("month", "day"), sep = 2, remove = T) %>% 
  mutate(., FID = paste(FileName, ResultStartSeconds, sep = "_")) %>% 
  mutate_at(vars(2:18), scale)

point_id <- "SERF"
point_id_lower <- "serf"
index_name <- "EventsPerSecond"
index_abb <- "EPS"
month <- 3
year <- 2013

test <- filter(indices, year == year & month == month)

test <- indices %>% 
  filter(., year == 2013 & month == 3) %>%
  with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
  select(., all_of(index_name)) %>% 
  write.table(., getDataPath("STSC", paste(point_id, index_name, year, month, ".txt", sep = "")), row.names = F, col.names = F)

test <- indices %>% 
  filter(., year == year & month == month) %>%
  with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
  select(., AcousticComplexity, EventsPerSecond, TemporalEntropy, FileName, location, date, time, ResultStartSeconds, ResultMinute, FID, year, month, day) %>% 
  write.csv(., getDataPath("STSC", "Results", point_id, paste(point_id, year, month, ".csv", sep = "")))
