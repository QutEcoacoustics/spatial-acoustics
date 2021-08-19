library(tidyverse)
library(seewave)
library(tuneR)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
sensor_point <- "WA01"
# location2 <- "WA011"
chapter <- "Chapter1_FineScaleAcousticSurvey"

field_data <- read.csv(getDataPath("Fieldwork_Bowra", "27.08.2019_Data.csv")) %>% 
  select(Point, location)


info1 <- read.csv(getDataPath(chapter, "27.02.2021_CompleteData.csv")) %>% 
  merge(., field_data, by.x = "point", by.y = "Point", all.x = T) %>% 
  mutate(folder = case_when(nchar(RECORDER) == 2 ~ paste(location, "_", "REC", RECORDER, sep = ""),
                            nchar(RECORDER) < 2 ~ paste(location, "_", "REC0", RECORDER, sep = ""))) %>% 
  rename(FileName_start = FileName) %>% 
  mutate(FileName_end = case_when(length + ResultMinute < 60 ~ as.character(FileName_start),
                                  length + ResultMinute == 60 ~ as.character(FileName_start),
                                  length + ResultMinute > 60 ~ case_when(time == 230155 ~ paste((date+1), "_000000", sep = ""),
                                                                         time == 110055 ~ paste(date, "_120100", sep = ""),
                                                                         nchar(time + 10005) == 6 ~  paste(date, "_", (time + 10005), sep = ""),
                                                                         nchar(time + 10005) < 6 ~ paste(date, "_0", (time + 10005), sep = "")))) %>% 
  select(point, position, index_value, ResultMinute, length, FileName_start, FileName_end, folder, id, everything())

# info <- info1[1:446,]

# if (file.exists(getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = ""))) {
#   print ("file exists")
#   
#   else
#     
# }
#     
#     )

list_missing <- as.data.frame(list_missing)

for (row in 1:nrow(info)) {
  
  if (file.exists(getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = "")))) {
    print ("") }
    
    else {
      print(paste(info$id[row], ".wav", sep = ""))
      
      } }

list_missing <- rename(list_missing, motif_id = `paste(info$id[row], ".wav", sep = "")`)


for (row in 1:nrow(info)) { 
  
  if (file.exists(getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = "")))) {
    print ("") }

else {
  print (paste(info$id[row], ".wav", sep = ""))
  
  w1 <- readWave(paste("Y:/Marina/Bowra/Aug2019/", info$folder[row], "/", info$FileName_start[row], ".wav", sep = ""))
  w2 <-  readWave(paste("Y:/Marina/Bowra/Aug2019/", info$folder[row], "/", info$FileName_end[row], ".wav", sep = ""))
  
  
  tryCatch({
  
  # case_when(info$FileName_start[1] == info$FileName_end[1] ~ 
  #             # readWave(filename = paste("Y:/Marina/Bowra/Aug2019/", info$folder[1], "/", info$FileName_start[1], ".wav", sep = "")) %>% 
  #             cutw(w1, f = 32000, from = (info$ResultMinute[1]*60), to = ((info$ResultMinute[1] + info$length[1])*60)) %>% 
  #             savewav(., f = 32000, filename = getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = ""))),
  #           info$FileName_start[1] != info$FileName_end[1] ~ 
              pastew(wave1 = w1, wave2 = w2, f = 32000) %>% 
    cutw(., f = 32000, from = (info$ResultMinute[row]*60), to = ((info$ResultMinute[row] + info$length[row])*60)) %>% 
    savewav(., f = 32000, filename = getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = ""))) },
    
    
     error = function(e) {skip_to_next <<-TRUE })
   
   if(skip_to_next) { next }
} 
  
}
  