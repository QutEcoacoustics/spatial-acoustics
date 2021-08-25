library(tidyverse)

rm(list = ls())

#Functions ----

getDataPath <- function (...) {
  return(file.path("D:/",  ...))
}

list_myfiles <- function(..., search_pattern) {
  files <- list.files(getDataPath(...), pattern = search_pattern, recursive = T, full.names = T)
  
}

create_mydir <- function(current_step) {
  dir.create(getDataPath(folder, current_step))
}

ordering_files <-
  function(original_dataframe,
           index_name) {
    if (is.null(index_name)) {
      c <- read.csv(original_dataframe) %>%
        with(., .[order(date_time, ResultMinute),])
      new_dataframe <- rbind(new_dataframe, c)
      write.csv(new_dataframe,
                getDataPath(
                  folder,
                  step2,
                  paste(site, "_", point, "_", month, ".csv", sep = "")
                ),
                row.names = F)
      
    } else {
      c <- read.csv(original_dataframe) %>%
        with(., .[order(date_time, ResultMinute),]) %>%
        select(., all_of(index_name))
      new_dataframe <- rbind(new_dataframe, c)
      write.table(
        new_dataframe,
        getDataPath(
          folder,
          step2,
          paste(site, "_", point, "_", month, "_", index_name, ".txt", sep = "")
        ),
        row.names = F,
        col.names = F
      )
    }
    
  }

step1 <- "1_IndicesToTs"
folder <- "Test"

#Listing AI files----

create_mydir(step1)

files <- list_myfiles(folder, search_pattern = ".Indices.csv")


####Creating dirs and building TS ----
#First create and unique identifier to each minute using the file name - which ideally has date and time embedded - and then scale the indices values - if necessary adjust the columns in line 15 - scale only columns with indices

#This one create one file with all the 3 indices + all variables needed for subsequence analysis. It also saves the sites, points and date_time attributes so we use it in the next step

site_id <- NULL
point_id <- NULL
date_time_id <- NULL
month_id <- NULL

for (file in files) {
  t <- strsplit(file, split = "/")
  f <- strsplit(t[[1]][4], split = "_") %>%
    .[[1]][2]
  u <- strsplit(t[[1]][7], split = "_")
  d <- substr(u[[1]][1], start = 1, stop = 6)
  
  site_id <- c(site_id, f[[1]][2])
  point_id <- c(point_id, t[[1]][5])
  date_time_id <- c(date_time_id, u[[1]][1])
  month_id <- c(month_id, d[[1]][1])
  
  site_id <- unique(site_id)
  point_id <- unique(point_id)
  date_time_id <- unique(date_time_id)
  month_id <- unique(month_id)
  
  #site_info <- c(site_info, f)
  
  if (dir.exists(f[[1]][2])) {
    cat("The folder already exists")
    
  } else {
    dir.create(getDataPath(folder, step1, f[[1]][2]))
    
  }
  
  if (dir.exists(t[[1]][8])) {
    cat("The folder already exists")
    
  } else {
    dir.create(getDataPath(folder, step1, f[[1]][2], t[[1]][5]))
    
  }
  
  
  read.csv(file) %>%
    mutate(., FID = paste(basename(file), ResultMinute, sep = "_")) %>%
    separate(
      .,
      FID,
      into = c("date_time", "useless", "useless1", "useless2"),
      sep = "_",
      remove = F
    ) %>%
    separate(
      .,
      date_time,
      into = c("date", "time"),
      sep = "T",
      remove = F
    ) %>%
    mutate(., site = f[[1]][2]) %>%
    mutate(., point = t[[1]][5]) %>%
    mutate(., filepath = file) %>%
    select(
      .,
      AcousticComplexity,
      EventsPerSecond,
      TemporalEntropy,
      FileName,
      date_time,
      date,
      time,
      ResultMinute,
      FID,
      site,
      point,
      filepath
    ) %>%
    mutate_at(vars(1:3), scale) %>%
    with(., .[order(as.numeric(date), as.numeric(time), ResultMinute), ]) %>%
    write.csv(., getDataPath(
      folder,
      step1,
      f[[1]][2],
      t[[1]][5],
      paste("1_", f[[1]][2], "_", t[[1]][5], "_", u[[1]][1], ".csv", sep = "")
    ), row.names = F)
}

#Creating monthly dfs/per index  ----


files <- list_myfiles(c(folder, step1), search_pattern = "1_")


step2 <- "2_MonthlyAI"

create_mydir(step2)

new_dataframe <- NULL


for (file in files) {
  
    for(site in site_id) {
      for (point in point_id) {
        for (month in month_id) {
          
        
          ordering_files(file, index_name = NULL)
          ordering_files(file, index_name = "AcousticComplexity")
          ordering_files(file, index_name = "TemporalEntropy")
          ordering_files(file, index_name = "EventsPerSecond")
      }
    }
  
    }
}


#Running HIME ----


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
