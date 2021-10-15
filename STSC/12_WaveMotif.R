library(tidyverse)
library(seewave)
library(tuneR)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

site <- "2"
point <- "6"
geo_id <- paste(site, point, sep = "_")
#sensor_point <- "WA01"
# location2 <- "WA011"
chapter <- "AIndices"

dir.create(paste("D:", chapter, "motifs", site, sep = "/"))
dir.create(paste("D:", chapter, "motifs", site, point, sep = "/"))

data <- read.csv(getDataPath(chapter, "8_FeatureExtraction", "2_6_202008_wavelet.csv")) %>% 
  filter(class == "?") %>% 
  droplevels(.)


info <- read.csv(getDataPath(chapter, "6_CompleteMotif", "2_6_202008_motif_complete.csv")) %>% 
  merge(., data, by.x = "id", by.y = "id", all.y = T) %>% 
  distinct(id, .keep_all = T) %>% 
  #mutate(folder = case_when(nchar(RECORDER) == 2 ~ paste(location, "_", "REC", RECORDER, sep = ""),
                           # nchar(RECORDER) < 2 ~ paste(location, "_", "REC0", RECORDER, sep = ""))) %>% 
  rename(FileName_start = FileName) %>% 
  separate(time, into = c("time_real", "timezone_offset"), remove = F) %>%
  #mutate(time_real = as.integer(time_real)) %>% 
  mutate(FileName_end = case_when(length + ResultMinute < 120 ~ as.character(FileName_start),
                                  length + ResultMinute == 120 ~ as.character(FileName_start),
                                  length + ResultMinute > 120 ~ case_when(as.numeric(time_real) == '220000' ~ paste((date+1), "T000000+0930_REC", sep = ""),
                                                                         #time == 110055 ~ paste(date, "_120100", sep = ""),
                                                                         as.integer(time_real) >= 080000 ~ paste(date, "T", as.integer((as.integer(time_real) + 20000)), "+0930_REC", sep = ""),
                                  as.integer(time_real) <= 080000 ~ paste(date, "T0", as.integer((as.integer(time_real) + 20000)), "+0930_REC", sep = "")))) %>% 
  
                                                                         #nchar(time + 10000) < 6 ~ paste(date, "_0", (time + 20000), sep = "")))) %>% 
  select(point, position, index_value, ResultMinute, length, FileName_start, FileName_end, id, everything()) %>% 
  droplevels(.)

info$FileName_end <- as.factor(info$FileName_end)

# info <- info1[1:446,]

# if (file.exists(getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = ""))) {
#   print ("file exists")
#   
#   else
#     
# }
#     
#     )

# list_missing <- as.data.frame(list_missing)
# 
# for (row in 1:nrow(info)) {
#   
#   if (file.exists(getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = "")))) {
#     print ("") }
#     
#     else {
#       print(paste(info$id[row], ".wav", sep = ""))
#       
#       } }
# 
# list_missing <- rename(list_missing, motif_id = `paste(info$id[row], ".wav", sep = "")`)
flac2wave <- pivot_longer(info, cols = c(FileName_start, FileName_end), names_to = "info") %>% 
  distinct(value, .keep_all = T) %>% 
  select(info, value, date, everything())

#flac2wave <- flac2wave[1:7,]

for (row in 1:nrow(flac2wave)) { 
  if (file.exists(filename = paste("C:/Recs/2/6", "/", flac2wave$value[row], ".wav", sep = ""))) {
      print(paste(flac2wave$value[row], "exists", sep = " ")) 
  } else {
      print(paste("transforming", flac2wave$value[row], sep = " "))
      wav2flac(paste("C:/Recs/2/6", "/", flac2wave$value[2], ".flac", sep = ""), reverse = T, overwrite = F, path2exe = "C:/Users/n10393021/Downloads/flac-1.3.1pre1-win/flac-1.3.1pre1-win/win64") 
      }
  }


#   if (file.exists(filename = paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_start[row], ".wav", sep = ""))) {
#     
#     print (info$FileName_start[row]) }
#   
#   else {
#     if (file.exists(filename = paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_end[row], ".wav", sep = ""))) {
#       print(info$FileName_end[row]) }
#     else {
#       if (info$FileName_start[row] == info$FileName_end[row]) {
#         print ("transforming start")
#         wav2flac(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_start[row], ".flac", sep = ""), reverse = T, overwrite = F) }
#       
#       else {
#         print ("transforming start + end")
#         wav2flac(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_start[row], ".flac", sep = ""), reverse = T, overwrite = F)
#         wav2flac(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_end[row], ".flac", sep = ""), reverse = T, overwrite = F) } }}}
# 
# 
# 
# 
for (row in 1:nrow(info)) {

  if (file.exists(filename = paste("D:/AIndices/motifs", info$site[row], info$point[row], info$id[row], ".wav", sep = "/"))) {
    print ("") 
    } else {
  print (paste(info$id[row], ".wav", sep = ""))
  
  # wav2flac(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_start[row], ".flac", sep = ""), reverse = T, overwrite = F)
  # wav2flac(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_end[row], ".flac", sep = ""), reverse = T, overwrite = F)
  
  tryCatch({
    
    w1 <- readWave(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_start[row], ".wav", sep = ""))
    w2 <-  readWave(paste("T:/Marina/a2o/20210603_2/6/335/", paste(info$date[row], "_AAO", sep = ""), "/", info$FileName_end[row], ".wav", sep = ""))
  
  # case_when(info$FileName_start[1] == info$FileName_end[1] ~ 
  #             # readWave(filename = paste("Y:/Marina/Bowra/Aug2019/", info$folder[1], "/", info$FileName_start[1], ".wav", sep = "")) %>% 
  #             cutw(w1, f = 32000, from = (info$ResultMinute[1]*60), to = ((info$ResultMinute[1] + info$length[1])*60)) %>% 
  #             savewav(., f = 32000, filename = getDataPath(chapter, "motifs_id", paste(info$id[row], ".wav", sep = ""))),
  #           info$FileName_start[1] != info$FileName_end[1] ~ 
   pastew(wave1 = w1, wave2 = w2, f = 22050) %>% 
    cutw(., f = 22050, from = (info$ResultMinute[row]*60), to = ((info$ResultMinute[row] + info$length[row])*60)) %>% 
    savewav(., f = 22050, filename = paste("D:/AIndices/motifs", '/', info$site[row], '/', info$point[row], '/', info$id[row], ".wav", sep = "")) },
    
    
     error = function(e) {skip_to_next <<-TRUE })
   
   if(skip_to_next) { next }
} 
  
}
  