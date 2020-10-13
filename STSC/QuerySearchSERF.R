library(dtwclust)
library(tidyverse)
library(ggplot2)
library(TSclust)
library(purrr)
library(data.table)

rm(list = ls())

cluster_method <- "hierarchical"
k <- 5
data <- "SERF"
batch <- 20153
#INDEX <- "aci"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}


list_matchfiles <- list.files(getDataPath("STSC", "Results"), pattern = "*20153_match.csv", recursive = T)
list_motiffiles <- list.files(getDataPath("STSC", "Results"), pattern = "*20153_motif.csv", recursive = T)

  
output_match <- data.frame(position =	integer(),
                           index_value = numeric(),
                           FileName = factor(),
                           date = integer(),
                           time = integer(),
                           ResultStartSeconds = integer(),
                           ResultMinute = integer(),
                           FID = character(),
                           distance = numeric(),
                           length = integer(),
                           reference = character(),
                           id	= character(),
                           fid_what = factor())

for (file in list_matchfiles) {
    file_result <- read.csv(getDataPath("STSC", "Results", file)) %>% 
    filter(., match != is.na(T)) %>% 
    rename(., fid = match) %>%
    rename(., index_value = Index) %>% 
    select(everything(), -motif) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_20153_match.csv", replacement = ""), fid, sep = "_"))
    output_match <- rbind(output_match, file_result)

}


output_motif <- data.frame(position = numeric(),
                           index_value = numeric(),
                           FileName = factor(),
                           date = integer(),
                           time = integer(),
                           ResultStartSeconds = integer(),
                           ResultMinute = integer(),
                           FID = character(),
                           distance = numeric(),
                           length = integer(),
                           reference = character(),
                           id	= character(),
                           fid_what = factor())

for (file in list_motiffiles) {
  file_result <- read.csv(getDataPath("STSC", "Results", file)) %>% 
    filter(., motif != is.na(T)) %>% 
    rename(., fid = motif) %>%
    rename(., index_value = Index) %>% 
    mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "_20153_motif.csv", replacement = ""), fid, sep = "_"))
  output_motif <- rbind(output_motif, file_result)
  
}

df <- rbind(output_match, output_motif) %>% 
  separate(., col = id, into = c("point", "index_name", "number", "what"), sep = "_", remove = F)

ts_clust <- select(df, index_value, id, position) %>%
  select(., id, index_value, position) %>% 
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

cluster <- tsclust(ts_list, type = cluster_method, seed = 123, distance = "dtw", k = k)
cvi(cluster, type = "valid")
plot(cluster)
print(cluster)

indices <- rbind(output_match, output_motif)
 
cluster_numbers <- as.data.frame(cbind(cluster@cluster, ts_clust)) %>% 
  rename("cluster_number" = "cluster@cluster") %>%
  select(cluster_number) %>%
  rownames_to_column(., "id")

cluster_assignment <- left_join(indices, cluster_numbers) %>%
  write.csv(., getDataPath("STSC", "Results", paste(data, batch, cluster_method, k, ".csv", sep = "")), row.names = F)

# #motif_complete <- data.frame(position =	integer(),
#                              #index_value = numeric(),
#                              #FileName = factor(),
#                              #date = integer(),
#                              #time = integer(),
#                              #ResultStartSeconds = integer(),
#                              #ResultMinute = integer(),
#                              #FID = character(),
#                              #distance = numeric(),
#                              #length = integer(),
#                              #reference = character(),
#                              #id	= character(),
#                              #fid_what = factor())
# 
# #for (file in list_motiffiles) {
#   #file_result <- read.csv(getDataPath("STSC", "Results", file)) %>% 
#     #filter(., motif != is.na(T)) %>% 
#     #rename(., fid_what = motif) %>%
#     #rename(., index_value = Index) %>% 
#     #mutate(., id = paste(basename(file) %>% 
#                            #gsub(pattern = "_motif.csv", replacement = ""), fid_what, sep = "_")) %>% 
#     #select(., position, index_value, FileName, date, time, ResultStartSeconds, ResultMinute, FID, distance, length, reference, id, fid_what)
#   #motif_complete <- rbind(motif_complete, file_result)
#   
# #}

#match_complete <- data.frame(position =	integer(),
#                              index_value = numeric(),
#                              FileName = factor(),
#                              date = integer(),
#                              time = integer(),
#                              ResultStartSeconds = integer(),
#                              ResultMinute = integer(),
#                              FID = character(),
#                              distance = numeric(),
#                              length = integer(),
#                              reference = character(),
#                              id	= character(),
#                              fid_what = factor())
# 
# for (file in list_matchfiles) {
#   file_result <- read.csv(getDataPath("STSC", "Results", file)) %>% 
#     filter(., match != is.na(T)) %>% 
#     rename(., fid_what = match) %>%
#     rename(., index_value = Index) %>% 
#     mutate(., id = paste(basename(file) %>% 
#                            gsub(pattern = "_match_20200922.csv", replacement = ""), fid_what, sep = "_")) %>% 
#     select(., position, index_value, FileName, date, time, ResultStartSeconds, ResultMinute, FID, distance, length, reference, id, fid_what)
#   match_complete <- rbind(match_complete, file_result)
#   
# }
# 
# 
# indices <- rbind(match_complete, motif_complete)
# 
# cluster_numbers <- as.data.frame(cbind(cluster@cluster, ts_clust)) %>% 
#   rename("cluster_number" = "cluster@cluster") %>% 
#   select(cluster_number) %>% 
#   rownames_to_column(., "id")
# 
# cluster_assignment <- left_join(indices, cluster_numbers) %>% 
#   write.csv(., getDataPath("STSC", "Results", paste(data, batch, cluster_method, k, ".csv", sep = "")), row.names = F)
# 
# 
