library(tidyverse)
library(magick)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
#point <- "WA003"

field_data <- read.csv(getDataPath("Fieldwork_Bowra", "27.08.2019_Data.csv"))

motif_result <- read.csv(getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = ""))) %>%
  separate(id, into = c("point", "index_name", "motif_number", "what"), remove = F) %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>%
  select(everything(), -c(position)) %>%
  mutate(image_file = paste(FileName, "__", index_name, ".png", sep = "")) %>% 
  group_by(id) %>% 
  filter(ResultMinute == min(ResultMinute))


for (row in 1:nrow(motif_result)) {
  list.files(getDataPath("Fieldwork_Bowra", "Aug2019", "Aug2019_concatfiles", motif_result$point[row]), pattern = motif_result$image_file[row], recursive = T, full.names = T) %>% 
  image_read(.) %>% 
    image_crop(., geometry_area(height = 256, width = motif_result$length[row]-(1-motif_result$ResultMinute[row]), y_off = 20, x_off = motif_result$ResultMinute[row])) %>% 
    image_write(., getDataPath("STSC", "Test", "Figures", paste(motif_result$point[row], motif_result$fid_what[row], motif_result$image_file[row], sep = "_")))
}



