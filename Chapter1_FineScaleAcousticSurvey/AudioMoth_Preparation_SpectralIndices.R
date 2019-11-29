library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra",  ...))
}

rm(list = ls())

#Read the table with the info about the minutes - good and bad, etc
InfoMinutes <- read.csv(getDataPath("Aug2019_Outputs_SpectralIndices", "ExcludedLinesPerRec.csv"))

bad_minutes <- filter(InfoMinutes, status == "bad") %>% 
  select(., filename) %>%
  unique(.)
  lapply(., as.character)
  

#List the files and open the tables containing the actual data to be manipulated
library(reshape2)


files <- list.files(getDataPath("Aug2019", ""), pattern = "Indices.csv", recursive = T, full.names = T)
files1 <- as.list(files) %>% 
  map(~ str_split(., pattern = "/")) %>% 
  as.data.frame(.)
  
sites <- files1[10,] %>% 
  unique(.)
  

str_match(sites, pattern = bad_minutes$site)

 
files %>% pluck(files, bad_minutes)

for (file in files) {
  
}

file1 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/JZ001_WB28/BOW-JZ1-WB28_20191014_180000.wav/Towsey.Acoustic/BOW-JZ1-WB28_20191014_180000__Towsey.Acoustic.R3D.csv")
file1 <- read.csv(file1) %>% 
  filter(Index == "2") %>% 
  mutate(., File = basename(file1))

file2 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/JZ002_WB11/BOW-JZ2-WB11_20191014_205938.wav/Towsey.Acoustic/BOW-JZ2-WB11_20191014_205938__Towsey.Acoustic.R3D.csv")
file2 <- read.csv(file2) %>% 
  filter(Index == "20") %>% 
  mutate(., File = basename(file2))

file3 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/JZ003_WB25/BOW-JZ3-WB25_20191015_065852.wav/Towsey.Acoustic/BOW-JZ3-WB25_20191015_065852__Towsey.Acoustic.R3D.csv")
file3 <- read.csv(file3) %>% 
  filter(Index == "18") %>% 
  mutate(., File = basename(file3))

file4 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM401_WB22/BOW-401-WB22_20191015_085838.wav/Towsey.Acoustic/BOW-401-WB22_20191015_085838__Towsey.Acoustic.R3D.csv")
file4 <- read.csv(file4) %>% 
  filter(Index == "33") %>% 
  mutate(., File = basename(file4))

file5 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM402_WB15/BOW-402-WB15_20191015_105827.wav/Towsey.Acoustic/BOW-402-WB15_20191015_105827__Towsey.Acoustic.R3D.csv")
file5 <- read.csv(file5) %>% 
  filter(Index == "14") %>% 
  mutate(., File = basename(file5))

file6 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM403_WB49/BOW-403-WB49_20191015_125827.wav/Towsey.Acoustic/BOW-403-WB49_20191015_125827__Towsey.Acoustic.R3D.csv")
file6 <- read.csv(file6) %>% 
  filter(Index == "51") %>% 
  mutate(., File = basename(file6))

file7 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM404_WB56/BOW-404-WB56_20191015_145807.wav/Towsey.Acoustic/BOW-404-WB56_20191015_145807__Towsey.Acoustic.R3D.csv")
file7 <- read.csv(file7) %>% 
  filter(Index == "56")%>% 
  mutate(., File = basename(file7))

file8 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM406_WB46/BOW-406-WB46_20191015_160002.wav/Towsey.Acoustic/BOW-406-WB46_20191015_160002__Towsey.Acoustic.R3D.csv")
file8 <- read.csv(file8) %>% 
  filter(Index == "51") %>% 
  mutate(., File = basename(file8))

file9 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM410_WB43/BOW-410-WB43_20191015_165807.wav/Towsey.Acoustic/BOW-410-WB43_20191015_165807__Towsey.Acoustic.R3D.csv") 
file9 <- read.csv(file9) %>% 
  filter(Index == "45") %>% 
  mutate(., File = basename(file9))

file10 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM411_WB35/BOW-411-WB35_20191015_175952.wav/Towsey.Acoustic/BOW-411-WB35_20191015_175952__Towsey.Acoustic.R3D.csv")
file10 <- read.csv(file10) %>% 
  filter(Index == "0") %>% 
  mutate(., File = basename(file10))

file11 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM412_WB34/BOW-412-WB34_20191016_065852.wav/Towsey.Acoustic/BOW-412-WB34_20191016_065852__Towsey.Acoustic.R3D.csv")
file11 <- read.csv(file11) %>% 
  filter(Index == "14") %>% 
  mutate(., File = basename(file11))

file12 <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/SM413_WB06/BOW-413-WB06_20191015_075847.wav/Towsey.Acoustic/BOW-413-WB06_20191015_075847__Towsey.Acoustic.R3D.csv")
file12 <- read.csv(file12) %>% 
  filter(Index == "32") %>% 
  mutate(., File = basename(file12))

#Rbind all the selected minutes in a csv file (this is done by index)

selected_minutes <- rbind(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10, file11, file12) %>% 
  select(., Index, File, everything()) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/WindRemoval_SpectralIndices_Channel1/R3D_SelectedMinutes.csv")


#Reading the full dataset with all spectral indices
df <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SpectralIndices_Channel1/AllSpectral_Channel1.csv")

#Separating the column FID into transect + point; date; time and index - which we will remove because there is already a column with this name. We are keeping the column FID because in there we can find all the info we need about the row
df1 <- separate(df, col = FID, into = c("TransectPoint", "Date", "Time", "Index2"), remove = F, sep = "_")
rm(df)

df2 <- select(df1, -c(X.2, X.1, X, Index2))
rm(df1)

norm_df <- df2 %>% mutate_at(vars(6:261), scale)
rm(df2)

t <- aggregate(norm_df, by = list(norm_df$TransectPoint, norm_df$Date, norm_df$Time, norm_df$Index), FUN = mean)

t1 <- select(t, -c(FID, TransectPoint, Date, Time))
rm(t)

t2 <- mutate(t1, FID = paste0(t1$Group.1, sep = "_", t1$Group.2, sep = "_", t1$Group.3, sep = "_", t1$Group.4, sep = "_", t1$Index))

t3 <- select(t2, -c(Group.1, Group.2, Group.3, Group.4, Index))

t4 <- select(t3, FID, everything())

rm(t1, t2, t3)

t5 <- data.frame(t4, row.names = 1)

t6 <- rowMeans(t5)


#write.csv(t5, "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/WindRemoval_SpectralIndices_Channel1/SpectralIndices_FCSAveragedPerMinute.csv")

write.csv(t5, getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SpectralIndices_FCSAveragedPerMinute.csv"))

#Creating the matrix with the euclidean distances of the points - huge one
m <- dist(t5, method = "euclidean")

#Transforming it into a mtrix
v <- data.matrix(m)

rm(t4)

#Using the reshape package to reshape the df

library(reshape2)

melted <- melt(v)

#selecting the windy minutes that were chosen to be the 0 point 

melted1 <- filter(melted, Var1 == "BOW-JZ1-WB28_20191014_180000_2_2" | Var1 == "BOW-JZ2-WB11_20191014_205938_20_20" | Var1 =="BOW-JZ3-WB25_20191015_065852_18_18" | Var1 == "BOW-401-WB22_20191015_085838_33_33" | Var1 == "BOW-402-WB15_20191015_105827_14_14" | Var1 == "BOW-403-WB49_20191015_125827_51_51" | Var1 == "BOW-404-WB56_20191015_145807_56_56" | Var1 == "BOW-406-WB46_20191015_160002_51_51" | Var1 == "BOW-410-WB43_20191015_165807_45_45" | Var1 == "BOW-411-WB35_20191015_175952_0_0" | Var1 == "BOW-412-WB34_20191016_065852_14_14" | Var1 == "BOW-413-WB06_20191015_075847_32_32")

write.csv(melted1, getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SpectralIndices_FCSPerSelectedMinute.csv"))

melted1 <- read.csv(getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SpectralIndices_FCSPerSelectedMinute.csv"))

df <- read.csv(getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SpectralIndices_FCSAveragedPerMinute.csv"))

head(melted1)

#Selecting the values of distance that were equal or less than .4

melted2 <- filter(melted1, value <= "0.4") %>% 
  write.csv(., getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SpectralIndices_WindMinAbove0.4.csv"))

x <- list(unique(melted2$Var2)) %>% 
  write.csv()

summary_all <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/indices_all.csv")

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel1/")

output_dir <- ("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/")

files <- list.files(directory, pattern = ".Indices.csv", full.names = T, recursive = T)

name <- basename(files)
name <- gsub(pattern = "__Towsey.Acoustic.Indices.csv", replacement = "", x = name)

files <- as.list(files)
df <- lapply(files, read.csv) %>% 
  lapply(files, mutate(FID =paste(FileName, ResultMinute, sep = "_")))

df <- select(df, BackgroundNoise, Snr, Activity, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, SptDensity, FileName, ResultMinute)

norm_df <- df %>% mutate_at(vars(1:15), scale)

library(stringr)

melted2 <- read.csv(getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SpectralIndices_WindMinAbove0.4.csv")) %>% 
  mutate(FID = str_sub(Var2, start = 1, end = 31))

windy_minutes <- as.list(unique(melted2$FID))

#Getting the summary Indices and eliminating the windy minutes, pasting it all together to build the distance matrix#
  
files <- list.files(directory, pattern = ".Indices.csv", full.names = T, recursive = T)

name <- basename(files)
name <- gsub(pattern = "__Towsey.Acoustic.Indices.csv", replacement = "", x = name)


files <- as.list(files)
df <- lapply(files, read.csv) %>% 
  map(~ mutate(., FID = paste(.$FileName, .$ResultMinute, sep = "_"))) %>% 
  map(~ mutate(., wind = match(FID, windy_minutes, nomatch = 0, incomparables = "NA"))) %>%
  map(~ filter(., wind == 0)) %>% 
  map(~ select(., BackgroundNoise, Snr, Activity, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, SptDensity, FileName, ResultMinute, wind, FID)) %>% 
  map(~ mutate_at(., vars(1:15), scale)) %>%
  map(~ separate(., col = FileName, into = c("Location", "Recorder", "PointData"), remove = F)) %>% 
  do.call(rbind, .) %>% 
  write.csv(., getDataPath("Oct2019", "WindRemoval_SpectralIndices_Channel1", "SummaryIndices_Channel1_WindRemoved.csv"))




 