library(tidyverse)
library(ggplot2)
library(stringr)
library(purrr)

rm(list = ls())

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults")

files <- list.files(directory, pattern = ".RVT.csv", full.names = T, recursive = T)



output_fragment <- gsub(x = files, pattern = directory, replacement = "")

for (file in files) {
  read.csv(file) %>% 
  #separate(., col = FileName, into = c("date", "time"), sep = "_", remove = F) %>%
  mutate(., "originalfile" = file) %>%
  #select(., -X, -X.1, -X.2, -X.3, -X.4) %>%
  separate(., col = originalfile, into = c("path", "location", "test1", "test2", "rec"), sep = "_", remove = F) %>% 
  #select(., -1, -X, -X.5, -1) %>% 
  separate(., col = location, into = c("Location", "Date"), sep = "/", remove = T) %>%
  select(., -path) %>%
  select(., -Date, -test2) %>% 
  separate(., col = test1, into = c("test1", "Transectpoint"), sep = "/", remove = T) %>% 
  select(., -test1) %>%
  mutate(., "Transect" = substr(Transectpoint, 1, 2)) %>% 
  mutate(., "Point" = substr(Transectpoint, 3, 5)) %>%
  mutate(., "indice" = "RVT") %>% 
  #select(., -X.4, -X.3, -X.2, -X.1, -X) %>% 
  #select(., -X.5) %>%
  
  write.csv(., file)
}

files <- as.list(files)
df <- lapply(files, read.csv) 
df<-do.call(rbind, df) 
write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_RVT.csv")

#Pasting spectral indices together

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices")

files <- list.files(directory, pattern = ".csv", full.names = T, recursive = T)

files <- as.list(files)
df <- lapply(files, read.csv) 
df<-do.call(rbind, df) 
write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_allspectralindices.csv")









