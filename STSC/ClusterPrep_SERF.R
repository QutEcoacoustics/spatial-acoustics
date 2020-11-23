library(tidyverse)
library(ggplot2)
library(purrr)
library(data.table)
library(BHC)

rm(list = ls())

# cluster_method <- "partitional"
# k = 5

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}


list_matchfiles <- list.files(getDataPath("STSC", "Results", "SERF"), pattern = "*_20153_match", recursive = T)
list_motiffiles <- list.files(getDataPath("STSC", "Results", "SERF"), pattern = "*_20153_motif", recursive = T)

  
# output_match <- data.frame(id = character(),
#              index_value = numeric(),
#              position = numeric())
# 
# for (file in list_matchfiles) {
#   file_result <- read.csv(getDataPath("STSC", "Results", "SERF", file)) %>% 
#     filter(., match != is.na(T)) %>% 
#     rename(., fid = match) %>%
#     rename(., index_value = Index) %>% 
#     mutate(., id = paste(basename(file) %>% 
#                            gsub(pattern = "*_20153_match.csv", replacement = ""), fid, sep = "_")) %>% 
#     dplyr::select(., id, index_value, position)
#   output_match <- rbind(output_match, file_result)
#   
# }
# 
# 
# output_motif <- data.frame(id = character(),
#                            index_value = numeric(),
#                            position = numeric())
# 
# for (file in list_motiffiles) {
#   file_result <- read.csv(getDataPath("STSC", "Results", "SERF", file)) %>% 
#     dplyr::filter(., motif != is.na(T)) %>% 
#     dplyr::rename(., fid = motif) %>%
#     dplyr::rename(., index_value = Index) %>% 
#     dplyr::mutate(., id = paste(basename(file) %>% 
#                            gsub(pattern = "*_20153_motif.csv", replacement = ""), fid, sep = "_")) %>% 
#     dplyr::select(., id, index_value, position)
#   output_motif <- rbind(output_motif, file_result)
#   
# }
# 
# df <- rbind(output_match, output_motif) %>% 
#   write.csv(., "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/Results/matchmotif_SERF_bind1.csv")
# 
# ts_clust <- dplyr::select(df, index_value, id, position) %>%
#   group_by(., id) %>%
#   mutate(., new_position = order(order(position))) %>%
#   ungroup(.) %>%
#   dplyr::select(., everything(), -position) %>%
#   pivot_wider(., names_from = new_position, values_from = index_value) %>%
#   as.data.frame(.)

# rownames(ts_clust) <- ts_clust$id
# ts_clust <- ts_clust[,2:length(ts_clust)]
# ts_list <- tslist(ts_clust) %>% 
#   map(., na.omit)
# 
# library(dtwclust)
# 
# cluster <- tsclust(ts_list, type = "hierarchical", seed = 123, distance = "dtw_basic", k = 2)
# cvi(cluster, type = "valid")
# plot(cluster)
# print(cluster)
# 
motif_complete <- data.frame(position =	integer(),
                             index_value = numeric(),
                             FileName = factor(),
                             location = character(),
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
  file_result <- read.csv(getDataPath("STSC", "Results", "SERF", file)) %>%
    dplyr::filter(., motif != is.na(T)) %>%
    dplyr::rename(., fid_what = motif) %>%
    dplyr::rename(., index_value = Index) %>%
    dplyr::mutate(., id = paste(basename(file) %>%
                           gsub(pattern = "*_20153_motif.csv", replacement = ""), fid_what, sep = "_")) %>%
    dplyr::select(., position, index_value, FileName, location, date, time, ResultStartSeconds, ResultMinute, FID, distance, length, reference, id, fid_what)
  motif_complete <- rbind(motif_complete, file_result)

}

match_complete <- data.frame(position =	integer(),
                             index_value = numeric(),
                             FileName = factor(),
                             location = character(),
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
  file_result <- read.csv(getDataPath("STSC", "Results", "SERF", file)) %>%
    dplyr::filter(., match != is.na(T)) %>%
    dplyr::rename(., fid_what = match) %>%
    dplyr::rename(., index_value = Index) %>%
    dplyr::mutate(., id = paste(basename(file) %>%
                           gsub(pattern = "*_20153_match.csv", replacement = ""), fid_what, sep = "_")) %>%
    dplyr::select(., position, index_value, FileName, location, date, time, ResultStartSeconds, ResultMinute, FID, distance, length, reference, id, fid_what)
  match_complete <- rbind(match_complete, file_result)

}


indices <- rbind(match_complete, motif_complete) %>% 
  write.csv(getDataPath("STSC", "Results", "matchmotif_SERF_complete.csv"))
# 
# cluster_numbers <- as.data.frame(cbind(cluster@cluster, ts_clust)) %>% 
#   rename("cluster_number" = "cluster@cluster") %>% 
#   select(cluster_number) %>% 
#   rownames_to_column(., "id")
# 
# cluster_assignment <- left_join(indices, cluster_numbers) %>% 
#   write.csv(., getDataPath("STSC", "Results", paste(cluster_method, k, ".csv", sep = "")), row.names = F)
# 
# 
