rm(list = ls())

library(ggplot2)
library(tidyverse)


getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "AIndices"

list_files <- list.files(getDataPath(chapter, "9_MergedDF"), pattern = "chp", full.names = T)

#Per point ----

for (file in list_files) {
  
  name <- basename(file) %>% 
    str_split(., pattern = "_")
  
  name <- paste(name[[1]][[1]], name[[1]][[2]], sep = "_")
  
  table <- read.csv(file) %>% 
    filter(class != "NA") %>%
    separate(., id, into = c("site", "point", "month", "index", "number", "what"), sep = "_", remove = F) %>% 
    select(., class, id, site, point, month, index, position, index_value, date, time, time_real, reference, fid_what, date_time)
  
  count <- group_by(table, class, index) %>%
    count(.)
  
  
  #write.csv(count, getDataPath(chapter, "SummaryResults", paste("17.11.2021_countdata_", basename(file), sep = "")))
  
  
  sum <- sum(count$n)
  
  nmax <- max(count$n)-10
  
  plot <- ggplot() +
    geom_col(data = count, aes(x = index, y = n, fill = class), position = "dodge") +
    theme_classic() +
    #scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
    annotate(geom = "text", x = 1, y = nmax, label = paste("Total labels: ", sum, sep = "")) +
    ggsave(getDataPath(chapter, "SummaryResults", "Figures", paste("17.11.2021_", name, "_countplot", ".png", sep = "")))  
  
  
}

#Per site ----

names <- basename(list_files) %>% 
  str_split(., pattern = "_") %>% 
  sapply(., "[[", 1) %>% 
  unique()


for (name in names) {
  
  new_files <- list.files(getDataPath(chapter, "9_MergedDF"), pattern = glob2rx(paste(name, "_*chp*csv", sep = "")), full.names = T)
  
  # name <- basename(file) %>% 
  #   str_split(., pattern = "_")
  # 
  # name <- paste(name[[1]][[1]], sep = "_")
  
    
    table1 <- read.csv(new_files[1]) %>% 
      filter(class != "NA") %>%
      separate(., id, into = c("site", "point", "month", "index", "number", "what"), sep = "_", remove = F) %>% 
      select(., class, id, site, point, month, index, position, index_value, date, time, time_real, reference, fid_what, date_time)
    
    table2 <- read.csv(new_files[2]) %>% 
      filter(class != "NA") %>%
      separate(., id, into = c("site", "point", "month", "index", "number", "what"), sep = "_", remove = F) %>% 
      select(., class, id, site, point, month, index, position, index_value, date, time, time_real, reference, fid_what, date_time)
    
    table <- rbind(table1, table2)
    
    count <- group_by(table, class, index) %>%
      count(.)
    
    
    #write.csv(count, getDataPath(chapter, "SummaryResults", paste("17.11.2021_countdata_", basename(file), sep = "")))
    
    
    sum <- sum(count$n)
    
    nmax <- max(count$n)-10
    
    plot <- ggplot() +
      geom_col(data = count, aes(x = index, y = n, fill = class), position = "dodge") +
      theme_classic() +
      #scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
      annotate(geom = "text", x = 1, y = nmax, label = paste("Total labels: ", sum, sep = "")) +
      ggsave(getDataPath(chapter, "SummaryResults", "Figures", paste("21.01.2021_", name, "_countplot", ".png", sep = "")))  
    
    
  }

