#Cluster inspection

library(ggplot2)
library(tidyverse)
library(magick)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

cluster_results <- read.csv(getDataPath("STSC", "Results", "matchmotif_bow_complete.csv")) %>%
  tidyr::separate(id, into = c("point1", "index_name", "motif_number", "what"), remove = F) %>% 
  dplyr::group_by(., id) %>% 
  dplyr::mutate(., new_position = order(order(position))) %>% 
  dplyr::ungroup(.) %>%
  dplyr::select(everything(), -c(point1, position)) %>%
  dplyr::mutate(image_file = paste(FileName, "__", index_name, ".png", sep = "")) %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(ResultMinute == min(ResultMinute))


for (row in 1:nrow(cluster_results)) {
  list.files(getDataPath("Fieldwork_Bowra", "Oct2019", "ResultsIndices_Channel1"), pattern = cluster_results$image_file[row], recursive = T, full.names = T) %>% 
  image_read(.) %>% 
    image_crop(., geometry_area(height = 256, width = cluster_results$length[row]-(1-cluster_results$ResultMinute[row]), y_off = 20, x_off = cluster_results$ResultMinute[row])) %>% 
    image_write(., getDataPath("Chapter1_FineScaleAcousticSurvey", "STSC_GreySpectrograms", "DiscriminantAnalysis", paste(cluster_results$id[row], cluster_results$image_file[row], sep = "_")))
}


for (row in 1:nrow(cluster_results)) {
  cluster_results <- mutate(cluster_results, name_img_file[row] = paste(cluster_results$fid_what[row], cluster_results$image_file[row], sep = "_")) 
}

sampling <- paste(cluster_results$fid_what, cluster_results$image_file, sep = "_") %>% 
  sample(., size = 25)
