library(tidyverse)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "indices_all_AM.csv"))



df_order <- df %>% 
  #filter(., year == "2015" & month == "3") %>% 
  with(., .[order(date, time, ResultMinute),]) %>% 
  select(., AcousticComplexity, EventsPerSecond, TemporalEntropy)

write.csv(df_order, getDataPath("STSC", "Bowra", "201503_SERF.csv"))

df_index <- select(df_order, AcousticComplexity) %>% 
  write.table(., getDataPath("STSC", "201503_SERF_ACI.txt"), row.names = FALSE, col.names = FALSE)
