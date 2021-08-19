library(tidyverse)
library(magick)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "SERF"

chapter <- "Chapter2_SoundscapeTemporalAssessment"
#point <- "WA003"


motif_result <- read.csv(getDataPath(chapter, "Results", "4_MotifComplete", paste(data, "motif_complete.csv", sep = ""))) %>%
  separate(id, into = c("month", "index_name", "motif_number", "what"), remove = F) %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>%
  select(everything(), -c(position)) %>%
  mutate(image_file = paste(FileName, "__", index_name, ".png", sep = "")) %>% 
  group_by(id) %>% 
  filter(ResultMinute == min(ResultMinute)) %>% 
  droplevels(.)

motif_result$FileName <- as.character(motif_result$FileName)

subset <- motif_result[1:15,]

for (row in 1:nrow(motif_result)) {
  list.files(getDataPath(chapter, "SERF_AI", "1year_SERF"), pattern = motif_result$image_file[row], recursive = T, full.names = T) %>% 
  image_read(.) %>% 
    image_crop(., geometry_area(height = 256, width = motif_result$length[row]-(1-motif_result$ResultMinute[row]), y_off = 20, x_off = motif_result$ResultMinute[row])) %>% 
    image_write(., getDataPath(chapter, "Results", "5_CropSpectrogram", paste(motif_result$month[row], motif_result$fid_what[row], motif_result$image_file[row], sep = "_")))
}



