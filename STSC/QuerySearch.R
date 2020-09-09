library(rucrdtw)
library(dtwclust)
library(tidyverse)
library(ggplot2)
library(TSclust)
library(data.table)

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

df <- rbind(motif, match1)

row_names <- unique(df$fid)

ts_clust <- select(df, Index, position, fid) %>%
  pivot_wider(., names_from = position, values_from  = Index, values_fill = 0) %>%
  as.matrix(.)

rownames(ts_clust) <- row_names

ts_clust <- ts_clust[,2:264]

write.csv(ts_clust, getDataPath(point, index, "test.csv"))

ts_clust <- read.csv(getDataPath(point, index, "test.csv"), row.names = 1)
  
cluster <- tsclust(ts_clust, type = "hierarchical")
plot(cluster)


