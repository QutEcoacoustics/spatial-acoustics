library(dtwclust)
library(tidyverse)
library(ggplot2)
library(TSclust)
library(purrr)
library(factoextra)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

cluster_type = "hierarchical"


list_matchfiles <- list.files(getDataPath("Results"), pattern = "*_match_20200922.csv", recursive = T)
list_motiffiles <- list.files(getDataPath("Results"), pattern = "*_motif_20200922.csv", recursive = T)

  
output_match <- data.frame(id = character(),
                           index_value = numeric(),
                           position = numeric(),
                           FileName = character(),
                           location = character(),
                           rec = integer(),
                           point = character(),
                           date = integer(),
                           time = integer(),
                           ResultStartSeconds = integer(),
                           ResultMinute = integer(),
                           FID = character(),
                           distance = numeric(),
                           length = integer(),
                           reference = character(),
                           id = character(),
                           fid = character())

for (file in list_matchfiles) {
  file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., match != is.na(T)) %>% 
    rename(., fid = match) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>%  
                           gsub(pattern = "_match_20200922.csv", replacement = ""), fid, sep = "_")) %>% 
    select(everything(), -c(X, motif))
    output_match <- rbind(output_match, file_result)
    
}


output_motif <- data.frame(id = character(),
                           index_value = numeric(),
                           position = numeric(),
                           FileName = character(),
                           location = character(),
                           rec = integer(),
                           point = character(),
                           date = integer(),
                           time = integer(),
                           ResultStartSeconds = integer(),
                           ResultMinute = integer(),
                           FID = character(),
                           distance = numeric(),
                           length = integer(),
                           reference = character(),
                           id = character(),
                           fid = character())

for (file in list_motiffiles) {
  file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., motif != is.na(T)) %>% 
    rename(., fid = motif) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_motif_20200922.csv", replacement = ""), fid, sep = "_")) %>% 
    select(everything(), -X)
  output_motif <- rbind(output_motif, file_result)
  
}

df <- rbind(output_match, output_motif) %>% 
  separate(., col = id, into = c("point", "index_name", "number", "what"), sep = "_", remove = F)


all <- filter(df, index_name == "ENT" | index_name == "ACI") %>% 
  select(., index_value, id, position) %>%
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(all) <- all$id
all <- all[,2:length(all)]

ts_list <- tslist(all) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list, type = cluster_type, seed = 123, distance = "dtw", k = 4)
cvi(cluster, type = "valid")
print(cluster)
plot(cluster)

clustered_data_tidy <- as.data.frame(as.table(cluster@cluster))
colnames(clustered_data_tidy) <- c("id", "cluster")
clustered_data_tidy$id <- as.character(rownames(all))
joined_clusters <- df %>% inner_join(clustered_data_tidy, by = "id") %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  write.csv(., getDataPath("Results", "BOW_all_hierarchical4.csv"), row.names = F)

ENT_ACI <- filter(df, index_name == "ENT" | index_name == "ACI") %>% 
  select(., index_value, id, position) %>%
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(ENT_ACI) <- ENT_ACI$id
ENT_ACI <- ENT_ACI[,2:length(ENT_ACI)]

ts_list <- tslist(ENT_ACI) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list, type = cluster_type, seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
print(cluster)
plot(cluster)

clustered_data_tidy <- as.data.frame(as.table(cluster@cluster))
colnames(clustered_data_tidy) <- c("id", "cluster")
clustered_data_tidy$id <- as.character(rownames(ENT_ACI))
joined_clusters <- df %>% inner_join(clustered_data_tidy, by = "id") %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  write.csv(., getDataPath("Results", "BOW_ENT_ACI_hierarchical5.csv"))

EVN <- filter(df, index_name == "EVN") %>% 
  select(., index_value, id, position) %>%
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(EVN) <- EVN$id
EVN <- EVN[,2:length(EVN)]

ts_list <- tslist(EVN) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list, type = cluster_type, seed = 123, distance = "dtw", k = 2)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

clustered_data_tidy <- as.data.frame(as.table(cluster@cluster))
colnames(clustered_data_tidy) <- c("id", "cluster")
clustered_data_tidy$id <- as.character(rownames(EVN))
joined_clusters <- df %>% inner_join(clustered_data_tidy, by = "id") %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  write.csv(., getDataPath("Results", "BOW_EVN_hierarchical2.csv"))



