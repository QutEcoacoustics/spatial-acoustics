library(tidyverse)
library(ggplot2)
library(purrr)
library(data.table)
library(BHC)

rm(list = ls())

# cluster_method <- "partitional"
# k = 5

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
#point <- "WBA2O"

#list_matchfiles <- list.files(getDataPath("STSC", "Results", "SERF"), pattern = "*_20153_match", recursive = T)
list_motiffiles <- list.files(getDataPath("STSC", "Results", data), pattern = "*_motif.csv", recursive = T)

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
  file_result <- read.csv(getDataPath("STSC", "Results", data, file)) %>%
    dplyr::filter(., motif != is.na(T)) %>%
    dplyr::rename(., fid_what = motif) %>%
    dplyr::rename(., index_value = Index) %>%
    dplyr::mutate(., id = paste(basename(file) %>% 
                           gsub(pattern = "*_motif.csv", replacement = ""), fid_what, sep = "_")) %>%
    dplyr::select(., position, index_value, FileName, date, time, ResultMinute, distance, length, reference, id, fid_what)
  motif_complete <- rbind(motif_complete, file_result)

}


indices <- write.csv(motif_complete, getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = "")))
