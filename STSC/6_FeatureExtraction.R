library(tidyverse)
library(caret)
library(repr)
library(data.table)
library(TTR)
library(forecast)
library(lubridate)
library(randomForest)
library(wavelets)
library(party)
library(vegan)
library(dtwclust)
library(purrr)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
#point <- "20153"


# df with the motifs and filenames

motifs <- read.csv(getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = "")))


# # extracting DWT coefficients (with Haar filter)
ts_data <- select(motifs, index_value, position, id) %>%
  group_by(., id) %>%
  mutate(., new_position = order(order(position))) %>%
  ungroup(.) %>%
  select(., everything(), -position) %>%
  pivot_wider(., names_from = new_position, values_from = index_value) %>%
  as.data.frame(.)


rownames(ts_data) <- ts_data$id
ts_data <- ts_data[,2:length(ts_data)]


ts_list <- transpose(ts_data) %>% 
  map(., na.omit) 


wtData <- NULL


for (i in ts_list) {
  
  wt <- dwt(i, filter="haar", boundary= "periodic")
  
  un <- as.data.frame(t(unlist(c(wt@W,wt@V[[wt@level]]))))
  
  wtData <- plyr::rbind.fill(wtData, un)
  
}

rownames(wtData) <- rownames(ts_data)

write.csv(wtData, getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_0.csv", sep = "")), row.names = T)
