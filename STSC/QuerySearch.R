library(dtwclust)
library(tidyverse)
library(ggplot2)
library(TSclust)
library(purrr)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}


list_matchfiles <- list.files(getDataPath("Results"), pattern = "*_match.csv", recursive = T)
list_motiffiles <- list.files(getDataPath("Results"), pattern = "*_motif.csv", recursive = T)

  
output_match <- data.frame(id = character(),
             index_value = numeric(),
             position = numeric())

for (file in list_matchfiles) {
    file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., match != is.na(T)) %>% 
    rename(., fid = match) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_match.csv", replacement = ""), fid, sep = "_")) %>% 
    select(., id, index_value, position)
    output_match <- rbind(output_match, file_result)

}


output_motif <- data.frame(id = character(),
                           index_value = numeric(),
                           position = numeric())

for (file in list_motiffiles) {
  file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., motif != is.na(T)) %>% 
    rename(., fid = motif) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_motif.csv", replacement = ""), fid, sep = "_")) %>% 
    select(., id, index_value, position)
  output_motif <- rbind(output_motif, file_result)
  
}

df <- rbind(output_match, output_motif)

ts_clust <- select(df, index_value, id, position) %>%
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(ts_clust) <- ts_clust$id
ts_clust <- ts_clust[,2:length(ts_clust)]

ts_list <- tslist(ts_clust) %>% 
  map(., na.omit)
  


cluster <- tsclust(ts_list, type = "hierarchical")
plot(cluster)

D1 <- diss(ts_list, "DTWARP") %>% 
  as.matrix(D1)
heatmap(D1)

validity <- cvi(cluster)

shape <- shape_extraction(ts_list)