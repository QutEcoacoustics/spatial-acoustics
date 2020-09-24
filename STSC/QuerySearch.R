library(dtwclust)
library(tidyverse)
library(ggplot2)
library(TSclust)
library(purrr)
library(data.table)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}


list_matchfiles <- list.files(getDataPath("Results"), pattern = "*_match_20200922.csv", recursive = T)
list_motiffiles <- list.files(getDataPath("Results"), pattern = "*_motif_20200922.csv", recursive = T)

  
output_match <- data.frame(id = character(),
             index_value = numeric(),
             position = numeric())

for (file in list_matchfiles) {
    file_result <- read.csv(getDataPath("Results", file)) %>% 
    filter(., match != is.na(T)) %>% 
    rename(., fid = match) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_match_20200922.csv", replacement = ""), fid, sep = "_")) %>% 
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
                           gsub(pattern = "_motif_20200922.csv", replacement = ""), fid, sep = "_")) %>% 
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

cluster <- tsclust(ts_list, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

new_df <- df %>% 
  separate(., col = id, into = c("point", "index_name", "number", "what"), sep = "_", remove = F)

ACI <- filter(new_df, index_name == "ACI") %>% 
  select(id, index_value, position) %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(ACI) <- ACI$id
ACI <- ACI[,2:length(ACI)]

ts_list_ACI <- tslist(ACI) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list_ACI, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

EVN <- filter(new_df, index_name == "EVN") %>% 
  select(id, index_value, position)%>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(EVN) <- EVN$id
EVN <- EVN[,2:length(EVN)]

ts_list_EVN <- tslist(EVN) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list_EVN, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

ENT <- filter(new_df, index_name == "ENT") %>% 
  select(id, index_value, position)%>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(ENT) <- ENT$id
ENT <- ENT[,2:length(ENT)]

ts_list_ENT <- tslist(ENT) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list_ENT, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

ENT_ACI <- filter(new_df, index_name == "ENT" | index_name == "ACI") %>% 
  select(id, index_value, position)%>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(ENT_ACI) <- ENT_ACI$id
ENT_ACI <- ENT_ACI[,2:length(ENT_ACI)]

ts_list_ENT_ACI <- tslist(ENT_ACI) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list_ENT_ACI, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)


EVN_ACI <- filter(new_df, index_name == "EVN" | index_name == "ACI") %>% 
  select(id, index_value, position)%>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(EVN_ACI) <- EVN_ACI$id
EVN_ACI <- EVN_ACI[,2:length(EVN_ACI)]

ts_list_EVN_ACI <- tslist(EVN_ACI) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list_EVN_ACI, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)


EVN_ENT <- filter(new_df, index_name == "EVN" | index_name == "ENT") %>% 
  select(id, index_value, position)%>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>% 
  select(., everything(), -position) %>% 
  pivot_wider(., names_from = new_position, values_from = index_value) %>% 
  as.data.frame(.)

rownames(EVN_ENT) <- EVN_ENT$id
EVN_ENT <- EVN_ENT[,2:length(EVN_ENT)]

ts_list_EVN_ENT <- tslist(EVN_ENT) %>% 
  map(., na.omit)

cluster <- tsclust(ts_list_EVN_ENT, type = "partitional", seed = 123, distance = "dtw", k = 5)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)


