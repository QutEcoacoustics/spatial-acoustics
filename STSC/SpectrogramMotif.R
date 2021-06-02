library(tidyverse)
# library(magick)
library(imager)
# library(grDevices)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
sensor_point <- "WB57"
# location2 <- "WA011"
chapter <- "Chapter1_FineScaleAcousticSurvey"

field_data <- read.csv(getDataPath("Fieldwork_Bowra", "27.08.2019_Data.csv")) %>% 
  select(Point, location)


motif_result_ <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>%
  # separate(id, into = c("point", "index_name", "motif_number", "what"), remove = F) %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>%
  select(everything(), -c(position)) %>%
  group_by(id) %>% 
  filter(ResultMinute == min(ResultMinute)) %>% 
  merge(., field_data, by.x = "point", by.y = "Point", all.x = T) %>% 
  # select(1:21) %>%
  mutate(image_file = paste(location, "_", "REC", RECORDER, "__", index, ".png", sep = ""))

motif_result <- filter(motif_result_, point == sensor_point)

#ACI

aci_motifs <- filter(motif_result, index == "ACI") %>%
  group_by(.$date) %>% 
  mutate(day = cur_group_id())

aci_img <- list.files(getDataPath("Fieldwork_Bowra", "Aug2019_concatfiles"), pattern = aci_motifs$image_file[1], recursive = T, full.names = T) %>%
  load.image(.)

for (row in 1:nrow(aci_motifs)) {
  # %>% 
  aci_img <- draw_rect(im = aci_img, x0 = ((((aci_motifs$day[row]*24)-24)+aci_motifs$hour[row]+1)*aci_motifs$ResultMinute[row]), y0 = 20, x1 = aci_motifs$length[row]-(1-((((aci_motifs$day[row]*24)-24)+aci_motifs$hour[row]+1)*aci_motifs$ResultMinute[row])), y1 = 276, opacity = 0.2, color = case_when(aci_motifs$class_model[row] == "bird" ~ "green",
                                                                                                                                                                     aci_motifs$class_model[row] == "insect" ~ "blue",
                                                                                                                                                                     TRUE ~ "black"))
  
  
}


save.image(aci_img, getDataPath(chapter, "Figures", "Spect_tagged", paste(aci_motifs$image_file[1], ".png", sep = "")))

#EVN

evn_motifs <- filter(motif_result, index == "EVN") %>%
  group_by(.$date) %>% 
  mutate(day = cur_group_id())

evn_img <- list.files(getDataPath("Fieldwork_Bowra", "Aug2019_concatfiles"), pattern = evn_motifs$image_file[1], recursive = T, full.names = T) %>%
  load.image(.)

for (row in 1:nrow(evn_motifs)) {
  # %>% 
  evn_img <- draw_rect(im = evn_img, x0 = ((((evn_motifs$day[row]*24)-24)+evn_motifs$hour[row]+1)*evn_motifs$ResultMinute[row]), y0 = 20, x1 = evn_motifs$length[row]-(1-((((evn_motifs$day[row]*24)-24)+evn_motifs$hour[row]+1)*evn_motifs$ResultMinute[row])), y1 = 276, opacity = 0.2, color = case_when(evn_motifs$class_model[row] == "bird" ~ "green",
                                                                                                                                                                                                                                                                                                            evn_motifs$class_model[row] == "insect" ~ "blue",
                                                                                                                                                                                                                                                                                                            TRUE ~ "black"))
  
  
}

save.image(evn_img, getDataPath(chapter, "Figures", "Spect_tagged", paste(evn_motifs$image_file[1], ".png", sep = "")))


#ENT

ent_motifs <- filter(motif_result, index == "ENT") %>%
  group_by(.$date) %>% 
  mutate(day = cur_group_id())

ent_img <- list.files(getDataPath("Fieldwork_Bowra", "Aug2019_concatfiles"), pattern = ent_motifs$image_file[1], recursive = T, full.names = T) %>%
  load.image(.)

for (row in 1:nrow(ent_motifs)) {
  # %>% 
  ent_img <- draw_rect(im = ent_img, x0 = ((((ent_motifs$day[row]*24)-24)+ent_motifs$hour[row]+1)*ent_motifs$ResultMinute[row]), y0 = 20, x1 = ent_motifs$length[row]-(1-((((ent_motifs$day[row]*24)-24)+ent_motifs$hour[row]+1)*ent_motifs$ResultMinute[row])), y1 = 276, opacity = 0.2, color = case_when(ent_motifs$class_model[row] == "bird" ~ "green",
                                                                                                                                                                                                                                                                                                            ent_motifs$class_model[row] == "insect" ~ "blue",
                                                                                                                                                                                                                                                                                                            TRUE ~ "black"))
  
  
}
plot(ent_img)

save.image(ent_img, getDataPath(chapter, "Figures", "Spect_tagged", paste(ent_motifs$image_file[1], ".png", sep = "")))


