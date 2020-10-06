#Cluster inspection

library(ggplot2)
library(tidyverse)
library(magick)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

cluster_results <- read.csv(getDataPath("STSC", "Results", "BOW_ENT_ACI_hierarchical5.csv")) %>%
  separate(id, into = c("point1", "index_name", "motif_number", "what")) %>% 
  #group_by(., fid_what) %>% 
  #mutate(., new_position = order(order(position))) %>% 
  #ungroup(.) %>%
  select(everything(), -c(point1, position)) %>%
  mutate(image_file = paste(FileName, "__", index_name, ".png", sep = "")) %>% 
  group_by(image_file) %>% 
  filter(ResultMinute == min(ResultMinute))


for (row in 1:nrow(cluster_results)) {
  list.files(getDataPath("Fieldwork_Bowra", "Oct2019", "ResultsIndices_Channel1"), pattern = cluster_results$image_file[row], recursive = T, full.names = T) %>% 
  image_read(.) %>% 
    image_crop(., geometry_area(height = 256, width = cluster_results$length[row]-(1-cluster_results$ResultMinute[row]), y_off = 20, x_off = cluster_results$ResultMinute[row])) %>% 
    image_write(., getDataPath("Chapter1_FineScaleAcousticSurvey", "STSC_GreySpectrograms", cluster_results$cluster_number[row], paste(cluster_results$fid_what[row], cluster_results$image_file[row], sep = "_")))
}

