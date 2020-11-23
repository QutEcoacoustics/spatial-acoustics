library(dtwclust)
library(tidyverse)
library(ggplot2)
library(TSclust)
library(purrr)
library(factoextra)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

cluster_type = "hierarchical"
k = 5

list_matchfiles <- list.files(getDataPath("Results"), pattern = "*_match_09102020.csv", recursive = T)
list_motiffiles <- list.files(getDataPath("Results"), pattern = "*_motif_09102020.csv", recursive = T)

  
output_match <- data.frame(id = character(),
             index_value = numeric(),
             position = numeric())

for (file in list_matchfiles) {
    file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., match != is.na(T)) %>% 
    rename(., fid_motif = match) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_match_09102020.csv", replacement = ""), FID, sep = "_")) %>% 
    select(., id, index_value, position)
    output_match <- rbind(output_match, file_result)

}


output_motif <- data.frame(id = character(),
                           index_value = numeric(),
                           position = numeric())

for (file in list_motiffiles) {
  file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., motif != is.na(T)) %>% 
    rename(., fid_motif = motif) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_motif_09102020.csv", replacement = ""), FID, sep = "_")) %>% 
    select(., id, index_value, position)
  output_motif <- rbind(output_motif, file_result)
  
}

df <- rbind(output_match, output_motif) %>% 
  separate(., col = id, into = c("point", "index_name", "number", "what"), sep = "_", remove = F)

WB06 <- filter(df, point == "WB06") %>% 
  select(., index_value, id, position) %>%
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(WB06) <- WB06$id
WB06 <- WB06[,2:length(WB06)]

ts_list <- tslist(WB06) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list, type = cluster_type, seed = 123, distance = "dtw", k = k)
cvi(cluster, type = "valid")
print(cluster)
plot(cluster)

clustered_data_tidy <- as.data.frame(as.table(cluster@cluster))
colnames(clustered_data_tidy) <- c("id", "cluster")
clustered_data_tidy$id <- as.character(clustered_data_tidy$id)
joined_clusters <- df %>% inner_join(clustered_data_tidy, by = "id") %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  write.csv(., getDataPath("Results", "BOW_ENT_ACI_hierarchical5.csv"), col.names = F)

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

cluster <- tsclust(ts_list, type = cluster_type, seed = 123, distance = "dtw", k = k)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

clustered_data_tidy <- as.data.frame(as.table(cluster@cluster))
colnames(clustered_data_tidy) <- c("id", "cluster")
clustered_data_tidy$id <- as.character(clustered_data_tidy$id)
joined_clusters <- df %>% inner_join(clustered_data_tidy, by = "id") %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  write.csv(., getDataPath("Results", "BOW_EVN_hierarchical5.csv"), col.names = F)



