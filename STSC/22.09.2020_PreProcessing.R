library(tidyverse)

rm(list = ls())

####Building TS

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

indices <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "indices_all1.csv")) %>% 
  separate(., col = FileName, into = c("location", "rec", "other"), sep = "-", remove = F) %>% 
  separate(., col = other, into = c("point", "date", "time"), sep = "_", remove = T) %>%
  mutate(., FID = paste(rec, FileName, ResultStartSeconds, sep = "_")) %>% 
  mutate_at(vars(6:23), scale)

point_id <- "WB56"
point_id_lower <- "wb56"
index_name <- "TemporalEntropy"
index_abb <- "ENT"

df_order <- indices %>% 
  filter(., point == point_id) %>%
  with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
  select(., all_of(index_name)) %>% 
  write.table(., getDataPath("STSC", paste(point_id, index_name, ".txt", sep = "")), row.names = F, col.names = F)

complete_df <- indices %>% 
  filter(., point == point_id) %>%
  with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
  select(., AcousticComplexity, EventsPerSecond, TemporalEntropy, FileName, location, rec, point, date, time, ResultStartSeconds, ResultMinute, FID) %>% 
  write.csv(., getDataPath("STSC", "Results", point_id, paste(point_id, ".csv", sep = "")))
