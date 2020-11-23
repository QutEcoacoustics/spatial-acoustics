library(tidyverse)
library(ggplot2)
library(stringr)
library(purrr)

rm(list = ls())

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019/")

output_dir <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_SummaryIndices/")

files <- list.files(directory, pattern = ".Indices.csv", full.names = T, recursive = T)

name <- basename(files)
name <- gsub(pattern = "__Towsey.Acoustic.Indices.csv", replacement = "", x = name)

df <- data.frame(BackgroundNoise = numeric(),
                 Snr = numeric(),
                 Activity = numeric(),
                 EventsPerSecond = numeric(),
                 HighFreqCover = numeric(),
                 MidFreqCover = numeric(),
                 LowFreqCover = numeric(),
                 AcousticComplexity = numeric(),
                 TemporalEntropy = numeric(),
                 EntropyOfAverageSpectrum = numeric(),
                 EntropyOfPeaksSpectrum = numeric(),
                 EntropyOfVarianceSpectrum = numeric(),
                 ClusterCount = numeric(),
                 Ndsi = numeric(),
                 SptDensity = numeric(),
                 FileName = factor(),
                 ResultMinute = integer(),
                 filepath = character(),
                 point = character(),
                 date = numeric(),
                 time = numeric())

for (file in files) {
 i <- read.csv(file) %>% 
    select(., BackgroundNoise, Snr, Activity, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, SptDensity, FileName, ResultMinute) %>% 
    mutate(., filepath = file) %>% 
    separate(., col = filepath, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "point", "j", "k", "l"), remove = F, sep = "/") %>% 
    separate(.,  col = FileName, into = c("date", "time"), sep = "_", remove = F) %>% 
    select(., -c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
  df <- rbind(i, df)
}

write.csv(df, paste(output_dir, "Bowraaug_indices_complete.csv", sep = "/"))

   #select(., -X, -X.1, -X.2, -X.3, -X.4) %>%
    # separate(., col = FileName, into = c("location", "rec", "other"), sep = "-", remove = F) %>% 
    # #select(., -1, -X, -X.5, -1) %>% 
    # separate(., col = other, into = c("point", "date", "time"), sep = "_", remove = T) %>%
    # select(., -path) %>%
    # select(., -Date, -test2) %>% 
    # separate(., col = test1, into = c("test1", "Transectpoint"), sep = "/", remove = T) %>% 
    # select(., -test1) %>%
    # mutate(., "Transect" = substr(Transectpoint, 1, 2)) %>% 
    # mutate(., "Point" = substr(Transectpoint, 3, 5)) %>% 
  #select(., -X.4, -X.3, -X.2, -X.1, -X) %>% 
  #select(., -X.5) %>%
  
    write.csv(paste0(output_dir, name,".csv",sep=""))
}

files <- as.list(files)
df <- lapply(files, read.csv) %>%
  map(., select, BackgroundNoise, Snr, Activity, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, SptDensity, FileName, ResultMinute) %>% 
  map(., mutate, filepath = files) %>% 
  map(., separate, col = filepath, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "point", "j", "k", "l"), remove = F, sep = "/") %>% 
  map(., separate, col = FileName, into = c("date", "time"), sep = "_", remove = F) %>% 
  map(., select, -c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
 df <- do.call(rbind, df)
  write.csv(., paste(output_dir, "Bowraaug_indices_complete.csv", sep = "/"))
  
g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_PreliminaryAnalysis_RecordingInterferences/test.csv")
g <- mutate(g, FID = paste(Transectpoint, rec, FileName, ResultStartSeconds, sep = "_"))
write.csv(g, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_SummaryIndices_Prepared/indices_all_AM.csv")


#Pasting HOBOS data and the indices df together

hobos <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_HOBOS/backup/Avrg10min.csv") %>% 
  mutate(., time = gsub(time, pattern = ":", replacement = "")) %>% 
  write.csv(., "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_HOBOS/backup/Avrg10min.csv")
  

#indices <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/test.csv") %>% 
  #separate(., col = date, into = c("year", "monthday"), by = ,4, remove = F) %>% 
  #separate(., col = monthday, into = c("month", "day"), by = ,2, remove = T)
#indices <- separate(indices, col = time, into = c("hour", "minseconds"), by = ,-4, remove = F) %>% 
  #write.csv(., "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/test_datetime.csv")

#indices <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/test_datetime.csv") %>% 
  #filter(time != "NA")
#write.csv(indices, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/test_datetime_NAremoved.csv")

indices <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/test_datetime_NAremoved.csv") %>% 
  #mutate_at(47, ~replace(., "A2O", 200))
  #mutate(., rec = gsub(rec, pattern = "REC", replacement = ""))
  
  #mutate_at(35, ~replace(., is.na(.), 0))

write.csv(indices, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/test_datetime_NAremoved.csv")


join <- right_join(hobos, indices)

library(vegan)

dataWB <- filter(indices, date == "20190813" & time >= "020010" | date == "20190814" | date == "20190815" & time >= "030015") %>%
  filter(., time != "41156")

dataWA <- filter(indices, date == "20190815" | date == "20190816" | date == "20190817") %>% 
  filter(., time != "41156")
 

dataWB <- filter(indices, date == "20190813" | date == "20190814" | date == "20190815") %>% 
  filter(., time != "41156")

head(data)


plot <- ggplot(dataWB, aes(x = time, y = EntropyOfVarianceSpectrum)) 
plot + geom_violin(aes(color = factor(rec)))+
facet_wrap(date~.) +
 # ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/Graphs/17.09.2019_EntropyOfVarianceSpectrum_WB.jpg") 

plot <- ggplot(data, aes(x = time, y = BackgroundNoise)) 
plot + geom_violin(aes(fill = day)) +
facet_wrap(rec~.)

plot <- ggplot(data, aes(x = time, y = EntropyOfCoVSpectrum)) 
plot + geom_violin(aes(fill = Point)) +
  facet_wrap(date~.)


plot <- ggplot(hobos, aes(x = time, y = Humidity))
plot + geom_violin(colour = point)


resultado <- rda(g[,8:25], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

databad2 <- filter(databad, date == c("20190815", "20190816", "20190817"))

b <- ggplot(databad2, aes(x = time, y = BackgroundNoise)) +
  geom_violin() +
  facet_wrap(date ~.)

refWA08 <- select(databad, FileName, time, date, ResultMinute, recorder, point, transect) 
data1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Aug2019_IndResults/WA018_REC08/20190815/WA018_REC08__Towsey.Acoustic.CVR.csv") %>% 
  select(., -Index) %>% 
  cbind(., refWA08) %>% 
  mutate(., "recorder" = "REC08", "point" = "018", "transect" = "WA") %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/PreliminaryAnalysis_RecordingInterferences/Tests/WA018_REC08_Towsey.Acoustic.CVR_MODIFIED.csv")



df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "SummaryIndices_Channel1_Prepared", "12.04.2020_gwrdata.csv"))

library(stringr)

df1 <- mutate(df, beginning_rec_modified1 = sprintf())

df <- mutate(df, rec_hour_mod = str_pad(beginning_rec_modified, width = 6, side = "left", pad = "0"))

df <- mutate(df, minute_mod = str_pad(ResultMinute, width = 2, side = "left", pad = "0"))


df <- mutate(df, hour_min_start = str_sub(rec_hour_mod, start = 1, end = 2))
df <- mutate(df, time_rec = paste(hour_min_start, minute_mod, sep = ""))
df <- mutate(df, time_rec_charac = str_pad(time_rec, width = 6, side = "right", pad = "0"))
df <- mutate(df, time_rec_numer = as.numeric(time_rec_charac))

write.csv(df2, getDataPath("Chapter1_FineScaleAcousticSurvey", "08.06.2020_completedata.csv"))



               