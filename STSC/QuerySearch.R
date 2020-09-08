library(rucrdtw)
library(dtwclust)
library(tidyverse)
library(ggplot2)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

point <- "WB06"
index <- "ACI"
motif_file <- "WB06_ACI_motif.csv"
match_file <- "WB06_ACI_match.csv"

data <- read.csv(getDataPath(point, index, motif_file))
match <- read.csv(getDataPath(point, index, match_file))

ts <- filter(data, reference == "0_ts") %>%
  select()
  #select(., Index) %>% 
  #as_vector(.)

motif <- filter(data, motif != is.na(T)) %>% 
  #separate(., motif, into = c("number", "what"), remove = F) %>% 
  rename(., fid = motif) #%>% 
  #select(., Index, fid, position) #%>% 
  #filter(., number == "113") %>% 
  #select(., Index) %>% 
  #as_vector(.)

match1 <- filter(match, match != is.na(T)) %>% 
  select(., -motif) %>% 
  #separate(., match, into = c("number", "what"), remove = F) %>% 
  rename(., fid = match) #%>% 
  #select(., Index, fid, position) #%>% 
  #filter(., number == "113") %>% 
  #select(., Index) %>% 
  #as_vector(.)

ts_clust <- rbind(motif, match1) 

ts34 <- filter(ts_clust, length == 34) %>% 
  select(., Index, fid, position) %>% 
  pivot_wider(., values_from = Index, names_from = position)

cluster <- tsclust(ts_clust, type = "hierarchical", k = c(2, 2, 4, 5, 6))
