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

#Removing the high frequency bands - above 6000 because of the click interference
#Each column corresponds to one freq bin (of approximately 43.1 Hz) and each line corresponds to one minute recording

p <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_SPT.csv")

d <- select(p, -(74:259))

write.csv(d, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/ChoppedFreq/spectralSPT_Freq0to6000.csv")

rm(d, p)

#Each column corresponds to one freq bin (of approximately 43.1 Hz) and each line corresponds to one minute recording

d <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/ChoppedFreq/spectralSPT_Freq0to6000.csv")


#Excluding files with interference - The mapping of the folers/files was made and this record is in a Spreadsheet (C:\Users\n10393021\OneDrive - Queensland University of Technology\Documents\PhD\Project\Fieldwork_Bowra\Outputs_SpectralIndices\ExcludedLinesPerRec.xlsx)

c <- filter(d, Transectpoint != "WA001" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA002" | Index >= 120 & Index <= 359 | Index >= 660 & Index <= 2219 | Index >= 2340 & Index <= 3959)
c <- filter(c, Transectpoint != "WA003" | Index >= 120 & Index <= 299 | Index >= 420 & Index <= 1259 | Index >= 1680 & Index <= 1859 | Index >= 1980 & Index <= 2639 | Index >= 2760 & Index <= 2999 | Index >= 3060 & Index <= 3479 | Index >= 3600 & Index <= 3659 | Index >= 3840 & Index <= 3959)
c <- filter(c, Transectpoint != "WA004" | Index >= 120 & Index <= 359 | Index >= 540 & Index <= 1979 | Index >= 2160 & Index <= 3539 | Index >= 3660 & Index <= 3959)
c <- filter(c, Transectpoint != "WA005" | Index >= 180 & Index <= 839 | Index >= 1020 & Index <= 2819 | Index >= 2940 & Index <= 4019)
c <- filter(c, Transectpoint != "WA006" | Index >= 120 & Index <= 779 | Index >= 960 & Index <= 3959)
c <- filter(c, Transectpoint != "WA007" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA008" | Index >= 180 & Index <= 359 | Index >= 540 & Index <= 1979 | Index >= 2160 & Index <= 3419)
c <- filter(c, Transectpoint != "WA009" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA011" | Index >= 120 & Index <= 479 | Index >= 1020 & Index <= 3959)
c <- filter(c, Transectpoint != "WA012" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA013" | Index >= 240 & Index <= 4079)
c <- filter(c, Transectpoint != "WA014" | Index >= 180 & Index <= 4019)
c <- filter(c, Transectpoint != "WA015" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA016" | Index >= 60 & Index <= 239| Index >= 300 & Index <= 1859 | Index >= 1920 & Index <= 3299 | Index >= 3540 & Index <= 3599 | Index >= 3720 & Index <= 3799 | Index >= 3840 & Index <= 3899)
c <- filter(c, Transectpoint != "WA017" | Index >= 120 & Index <= 1079| Index >= 1200 & Index <= 1499 | Index >= 1620 & Index <= 2639 | Index >= 2700 & Index <= 2999 | Index >= 3060 & Index <= 3959)
c <- filter(c, Transectpoint != "WA018" | Index >= 180 & Index <= 299| Index >= 420 & Index <= 1019 | Index >= 1140 & Index <= 1859 | Index >= 2040 & Index <= 2519 | Index >= 2580 & Index <= 3359 | Index >= 3600 & Index <= 3719 | Index >= 3840 & Index <= 3899)
c <- filter(c, Transectpoint != "WA019" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA020" | Index >= 180 & Index <= 299| Index >= 360 & Index <= 1019 | Index >= 1080 & Index <= 1859 | Index >= 1920 & Index <= 2519 | Index >= 2580 & Index <= 3299 | Index >= 3420 & Index <= 4019)
c <- filter(c, Transectpoint != "WA021" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA022" | Index >= 120 & Index <= 719| Index >= 900 & Index <= 3959)
c <- filter(c, Transectpoint != "WA023" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WA024" | Index >= 120 & Index <= 3959)
c <- filter(c, Transectpoint != "WAA2O" | Index >= 120 & Index <= 179| Index >= 300 & Index <= 1679 | Index >= 1800 & Index <= 3179 | Index >= 3240 & Index <= 3959)
c <- filter(c, Transectpoint != "WB006" | Index >= 120 & Index <= 2999)
c <- filter(c, Transectpoint != "WB008" | Index >= 0 & Index <= 1859 | Index >= 1920 & Index <= 2879)
c <- filter(c, Transectpoint != "WB011" | Index >= 180 & Index <= 3059)
c <- filter(c, Transectpoint != "WB014" | Index >= 480 & Index <= 1499 | Index >= 1740 & Index <= 2939)
c <- filter(c, Transectpoint != "WB015" | Index >= 120 & Index <= 479 | Index >= 540 & Index <= 1319 | Index >= 1440 & Index <= 1919 | Index >= 1980 & Index <= 2759 | Index >= 2820 & Index <= 2999)
c <- filter(c, Transectpoint != "WB017" | Index >= 240 & Index <= 1019 | Index >= 1440 & Index <= 2579 | Index >= 2880 & Index <= 3119)
c <- filter(c, Transectpoint != "WB024" | Index >= 240 & Index <= 3119)
c <- filter(c, Transectpoint != "WB025" | Index >= 120 & Index <= 2999)
c <- filter(c, Transectpoint != "WB027" | Index >= 60 & Index <= 539 | Index >= 660 & Index <= 1979 | Index >= 2160 & Index <= 2939)
c <- filter(c, Transectpoint != "WB028" | Index >= 60 & Index <= 359 | Index >= 420 & Index <= 2819 | Index >= 2880 & Index <= 2939)
c <- filter(c, Transectpoint != "WB029" | Index >= 60 & Index <= 479 | Index >= 540 & Index <= 1199 | Index >= 1320 & Index <= 1859 | Index >= 1980 & Index <= 2639 | Index >= 2700 & Index <= 2939)
c <- filter(c, Transectpoint != "WB034" | Index >= 120 & Index <= 599 | Index >= 1020 & Index <= 2279 | Index >= 2580 & Index <= 2999)
c <- filter(c, Transectpoint != "WB035" | Index >= 180 & Index <= 839 | Index >= 1260 & Index <= 1319 | Index >= 1380 & Index <= 2399 | Index >= 2820 & Index <= 3059)
c <- filter(c, Transectpoint != "WB036" | Index >= 120 & Index <= 539 | Index >= 660 & Index <= 1319 | Index >= 1380 & Index <= 2039 | Index >= 2160 & Index <= 2999)
c <- filter(c, Transectpoint != "WB043" | Index >= 120 & Index <= 2999)
c <- filter(c, Transectpoint != "WB044" | Index >= 0 & Index <= 779 | Index >= 1200 & Index <= 2339 | Index >= 2640 & Index <= 2879)
c <- filter(c, Transectpoint != "WB046" | Index >= 180 & Index <= 3059)
c <- filter(c, Transectpoint != "WB047" | Index >= 180 & Index <= 539 | Index >= 600 & Index <= 1379 | Index >= 1440 & Index <= 1979 | Index >= 2040 & Index <= 2819 | Index >= 2880 & Index <= 3059)
c <- filter(c, Transectpoint != "WB049" | Index >= 120 & Index <= 539 | Index >= 660 & Index <= 1259 | Index >= 1380 & Index <= 1979 | Index >= 2340 & Index <= 2459 | Index >= 2580 & Index <= 2639 | Index >= 2700 & Index <= 2999)
c <- filter(c, Transectpoint != "WB052" | Index >= 0 & Index <= 2879)
c <- filter(c, Transectpoint != "WB056" | Index >= 180 & Index <= 3059)
c <- filter(c, Transectpoint != "WB057" | Index >= 180 & Index <= 3059)
c <- filter(c, Transectpoint != "WBA2O" | Index >= 60 & Index <= 2939)

write.csv(c, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/Filtered_FreqTime/spectralSPT_FreqTimeFiltered.csv")

rm(c, d)

#Rbind dataframes

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/ChoppedFreq")

files <- list.files(directory, pattern = ".csv", full.names = T, recursive = T)

files <- as.list(files)
df <- lapply(files, read.csv) 
df<-do.call(rbind, df) 
write.csv(df, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/ChoppedFreq/AllSpectral_Freq0to6000.csv")

#Do the same for the other summary indices - generate them again?


