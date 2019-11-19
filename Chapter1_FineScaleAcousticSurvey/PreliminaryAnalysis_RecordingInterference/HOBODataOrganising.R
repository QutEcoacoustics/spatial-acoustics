library(tidyverse)

dir <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_HOBOS/backup")

dt <- read.csv("output/HOBOTEMPRH9_BOWRA_WB.csv") %>% 
  mutate(., "rec" = "13", "point" = "025", "transect" = "WB") %>% 
  write.csv(., "output/HOBOTEMPRH9_BOWRA_WB1.csv")

WB <- filter(data, day == c("13", "14", "15")) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_HOBOS/output/HOBOTEMPRH13_BOWRA_WB.csv")


WA <- filter(data, day == c("15", "16", "17")) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_HOBOS/output/HOBOTEMPRH13_BOWRA_WA.csv")

test <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_HOBOS/output/HOBOTEMPRH9_BOWRA_WB1.csv") %>% 
  separate(., col = time, into = c("hour", "minute", "seconds"), sep = ":", remove = F) %>% 
  group_by(., hour, day, rec, point, transect) %>% 
  #group_by(., time) %>% 
  summarise_at(vars(Humidity, Temperature), mean)

  
files <- list.files(dir, pattern = "1.csv", full.names = T, recursive = F)

output <- gsub(files, pattern = ".csv", replacement = "")
output <- paste(output, "average.csv", sep = "")


for (file in files) {
  read.csv(file) %>% 
  separate(., col = time, into = c("hour", "minute", "seconds"), sep = ":", remove = F) %>% 
    group_by(., hour, day, rec, point, transect) %>% 
    summarise_at(vars(Humidity, Temperature), mean) %>%  
    write.csv(., file)
}

files <- as.list(files)         
df <- lapply(files, read.csv)
df <- do.call(rbind, df) %>% 
  write.csv(., "Avrg.csv")