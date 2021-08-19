library(tidyverse)


rm(list = ls())



getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter2_SoundscapeTemporalAssessment"

files <- list.files(getDataPath(chapter, "SERF_AI", "1year_SERF"), pattern = ".Indices.csv", recursive = T)


####Building TS - First create and unique identifier to each minute using the file name - which ideally has date and time embedded - and then scale the indices values - if necessary adjust the columns in line 15 - scale only columns with indices.

#This one create one file with all the 3 indices + all variables needed for subsequence analysis.

for (file in files) {
  read.csv(getDataPath(chapter, "SERF_AI", "1year_SERF", file)) %>% 
    mutate(., FID = paste(FileName, ResultMinute, sep = "_")) %>% 
    separate(., FileName, into = c("location", "date", "time"), sep = "_", remove = F) %>% 
    select(., AcousticComplexity, EventsPerSecond, TemporalEntropy, FileName, date, time, ResultMinute, FID) %>% 
    mutate_at(vars(1:3), scale) %>% 
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    write.csv(., getDataPath(chapter, "AI_TS", basename(file)))
}
  

#Putting the data together by month

month <- "201703"
Location <- "SERF"

files2 <- list.files(getDataPath(chapter, "AI_TS"), pattern = paste("SAM-KWRA_", month, sep = ""), recursive = T)

#Creating .csv with monthly data

df <- NULL

for (file in files2) {
  c <-  read.csv(getDataPath(chapter, "AI_TS", file)) %>% 
    #filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),])
  df <- rbind(df, c) }
  
write.csv(df, getDataPath(chapter, "AI_TS_monthly", paste(month, Location, ".csv", sep = "")), row.names = F)

#Each index is an individual time series. To make it easier, we first assign index name, abbreviation and the location to create the time series for each location and index.

df <- NULL

index_name <- "AcousticComplexity"
index_abb <- "ACI"
Location <- "SERF"


for (file in files2) {
 c <-  read.csv(getDataPath(chapter, "AI_TS", file)) %>% 
    #filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name))
 df <- rbind(df, c)
  
}

write.table(df, getDataPath(chapter, "AI_TS_monthly", paste(month, index_name, Location, ".txt", sep = "")), row.names = F, col.names = F)

index_name <- "EventsPerSecond"
index_abb <- "EVN"
Location <- "SERF"

df <- NULL

for (file in files2) {
  c <-  read.csv(getDataPath(chapter, "AI_TS", file)) %>% 
    #filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name))
  df <- rbind(df, c)
    
  
}

write.table(df, getDataPath(chapter, "AI_TS_monthly", paste(month, index_name, Location, ".txt", sep = "")), row.names = F, col.names = F)

index_name <- "TemporalEntropy"
index_abb <- "ENT"
Location <- "SERF"

df <- NULL

for (file in files2) {
  c <-  read.csv(getDataPath(chapter, "AI_TS", file)) %>% 
    #filter(., point == p) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute),]) %>% 
    select(., all_of(index_name))
  df <- rbind(df, c)
  
  
}

write.table(df, getDataPath(chapter, "AI_TS_monthly", paste(month, index_name, Location, ".txt", sep = "")), row.names = F, col.names = F)




#After this step you should go to powershell and run the HIME algorithm to each time series
