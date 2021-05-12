library(tidyverse)
library(wavelets)
library(dtwclust)
library(purrr)
library(randomForest)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
#point <- "20153"


# df with the motifs and filenames

motifs <- read.csv(getDataPath("STSC", "Test", "Results", paste(data, "motif_complete.csv", sep = "")))


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


ts_list <- tslist(ts_data) %>%
  map(., na.omit)
 

wtData <- NULL


for (i in ts_list) {
  
  wt <- dwt(i, filter="haar", boundary= "periodic")
  
  un <- as.data.frame(t(unlist(c(wt@W,wt@V[[wt@level]]))))
  
  wtData <- plyr::rbind.fill(wtData, un)
  
}

wtData <- na.roughfix(wtData)

wtData$id <- rownames(ts_data)

wtData <- mutate(wtData, class = NA) %>% 
  mutate(., component = NA) %>% 
  select(., id, class, component, everything())


samples <- sample(wtData$id, size = ceiling(nrow(wtData)*0.30), replace = F)

write.csv(wtData, getDataPath("STSC", "Test", paste(data, "wavelet.csv", sep = "")), row.names = F)
write.csv(samples, getDataPath("STSC", "Test", paste(data, "labels_sample.csv", sep = "")), row.names = F)
