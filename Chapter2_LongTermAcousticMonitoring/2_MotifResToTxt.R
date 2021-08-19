library(tidyverse)

rm(list = ls())

####Building TS

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}


files <- list.files(getDataPath("STSC", "SERF", "Results_chp2"), pattern = "res", recursive = T)


####After motif - processing .txt file - you will need to store the results here in a new folder unless you don't mind the original ones to be overwritten - it is not 

for (file in files) {
    read.table(getDataPath("STSC", "SERF", "Results_chp2", file), sep = " ", blank.lines.skip = T, fileEncoding = "UTF-16") %>%
    select(., 2:7) %>% 
    write.table(., getDataPath("STSC", "SERF", "Results_chp2", file), row.names = F, col.names = F)
}


