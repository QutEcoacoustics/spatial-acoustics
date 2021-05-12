#Reconstructing point spectrograms

library(ggplot2)
library(tidyverse)
library(magick)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
#point <- "WA003"


motif_result <- read.csv(getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = ""))) %>%
  separate(id, into = c("point", "index_name", "motif_number", "what"), remove = F) %>% 
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>%
  select(everything(), -c(position)) %>%
  mutate(image_file = paste(FileName, "__", index_name, ".png", sep = "")) %>% 
  group_by(id) %>% 
  filter(ResultMinute == min(ResultMinute))

result <- filter(df_newveg1, class_model == "insect") %>% 
  # mutate(., new_position = order(order(position))) %>% 
  # ungroup(.) %>%
  # select(everything(), -c(position)) %>%
  separate(., id, into = c("id_point", "id_index_name", "id_motif_number", "id_what"), remove = F) %>%
  # group_by(., id) %>% 
  # mutate(., new_position = order(order(position))) %>% 
  # ungroup(.) %>%
  # select(everything(), -c(position)) %>%
  # mutate(., image_file = paste(id_point, "_", id_motif_number, "_", id_what, "_", FileName, "__", id_index_name, ".png", sep = "")) %>% 
  # group_by(id) %>% 
  # filter(ResultMinute == min(ResultMinute)) %>% 
  group_by(., id, index, Point) %>%
  arrange(date, position) %>%
  split(., list(.$Point#, .$index
                ))
  

# library(png)
# 
# for (row in 1:nrow(result)) {
#   files <- list.files(getDataPath("Chapter1_FineScaleAcousticSurvey", "STSC_GreySpectrograms", "DiscriminantAnalysis", "Bowraaug", pattern = result$image_file[1]), recursive = T, full.names = T)
# filelist <- lapply(files, readPNG)
# names(filelist) <- paste0(basename((files)))
# list2env(filelist, envir=.GlobalEnv) }
# 
# 
# par(mar=rep(0,4))
# layout(matrix(1:length(names(filelist)), ncol=test$n, byrow=TRUE))
# 
# 
# for(i in 1:length(names(filelist))) {
#   img <- readPNG(names(filelist[i]))
#   plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n")
#   rasterImage(img,0,0,1,1)
# }
# 
# result <- result1[1:3]
# 
# image
# 
# d <- NULL
# f <- NULL 

library(gifski)

for (i in 1:length(result)) { 
  for(j in 1:nrow(result[[i]]))
    {

  d <- list.files(getDataPath("Chapter1_FineScaleAcousticSurvey", "STSC_GreySpectrograms", "DiscriminantAnalysis", "Bowraaug"), pattern = glob2rx(pattern = paste(result[[i]]$id_point[j], "_", result[[i]]$id_motif_number[j], "_", result[[i]]$id_what[j], "_", "*__" ,  result[[i]]$id_index_name[j]
                                                                                                                                                                  , ".png", sep = "")), recursive = F, full.names = T) %>% 
    map(., image_read)
  }
  f <-  image_join(d, f)
    # map(., image_scale, geometry_area(width = 256, height = sum(result[[i]]$length))) %>% 
    # map(., image_append) %>% 
   image_write_gif(f, getDataPath("Chapter1_FineScaleAcousticSurvey", "STSC_GreySpectrograms", "DiscriminantAnalysis", "Bowraaug", "join", str_c(unique(result[[i]]$Point), "_"#, unique(result[[i]]$index)
                                                                                                                                                                                    ,"insects.gif", sep = "")), delay = 2)
}


  

print(had)

