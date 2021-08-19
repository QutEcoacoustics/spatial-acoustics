library(tidyverse)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

#Preparing a file with all the motifs for all points together and additional data necessary for further analysis

data <- "SERF"

chapter <- "Chapter2_SoundscapeTemporalAssessment"

list_motiffiles <- list.files(getDataPath(chapter, "Results", "3_ProcessingMotifs_cleaningup"), pattern = "*_motif.csv", recursive = T)

motif_complete <- data.frame(position =	integer(),
                             index_value = numeric(),
                             FileName = factor(),
                             date = integer(),
                             time = integer(),
                             ResultMinute = integer(),
                             FID = character(),
                             distance = numeric(),
                             length = integer(),
                             reference = character(),
                             id	= character(),
                             fid_what = factor())

for (file in list_motiffiles) {
  file_result <- read.csv(getDataPath(chapter, "Results", "3_ProcessingMotifs_cleaningup", file)) %>%
    dplyr::filter(., motif != is.na(T)) %>%
    dplyr::rename(., fid_what = motif) %>%
    dplyr::rename(., index_value = Index) %>%
    dplyr::mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "*_motif.csv", replacement = ""), fid_what, sep = "_")) %>%
    dplyr::select(., position, index_value, FileName, date, time, ResultMinute, distance, length, reference, id, fid_what)
  motif_complete <- rbind(motif_complete, file_result)

}


indices <- write.csv(motif_complete, getDataPath(chapter, "Results", "4_MotifComplete", paste(data, "motif_complete.csv", sep = "")), row.names = F)
