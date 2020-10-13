#Cluster inspection

library(ggplot2)
library(tidyverse)
library(magick)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

cluster_results <- read.csv(getDataPath("STSC", "Results", "SERF20153hierarchical5.csv")) %>%
  separate(id, into = c("point", "index_name", "motif_number", "what")) %>% 
  # group_by(., fid_what) %>% 
  # mutate(., new_position = order(order(position))) %>% 
  # ungroup(.) %>%
  select(everything(), -c(position)) %>%
  mutate(image_file = paste(FileName, "__", index_name, ".png", sep = "")) %>% 
  group_by(image_file) %>% 
  filter(ResultMinute == min(ResultMinute))


for (row in 1:nrow(cluster_results)) {
  list.files(getDataPath("Chapter2_SoundscapeTemporalAssessment", "SERF_AI", "2015"), pattern = cluster_results$image_file[row], recursive = T, full.names = T) %>% 
  image_read(.) %>% 
    image_crop(., geometry_area(height = 256, width = cluster_results$length[row]-(1-cluster_results$ResultMinute[row]), y_off = 20, x_off = cluster_results$ResultMinute[row])) %>% 
    image_write(., getDataPath("Chapter2_SoundscapeTemporalAssessment", "STSC_GreySpectrograms", cluster_results$cluster_number[row], paste(cluster_results$fid_what[row], cluster_results$image_file[row], sep = "_")))
}

