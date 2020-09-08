library(rucrdtw)
library(dtwclust)
library(tidyverse)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

point <- "WB06"
index <- "ACI"
motif_file <- "WB06_ACI_motif.csv"

data <- read.csv(getDataPath(point, index, motif_file))

ts <- filter(data, reference == "0_ts") %>% 
  select(., Index) %>% 
  as_vector(.)

motif <- filter(data, motif != is.na(T)) %>% 
  separate(., motif, into = c("number", "what"), remove = F) %>% 
  select(., Index, number, position) %>% 
  filter(., number == "113") %>% 
  select(., Index) %>% 
  as_vector(.)

window_size <- dtw_basic(ts, motif)
