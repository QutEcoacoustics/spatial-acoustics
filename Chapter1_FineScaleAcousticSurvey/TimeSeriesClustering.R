library(dtwclust)
library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

getDataPath <- function (...) {
  return(file.path("C:/Users/Nina Scarpelli/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra",  ...))
}



df <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "08.06.2020_completedata.csv"))

df <- mutate(df, rec_hour_mod = str_pad(beginning_rec_modified, width = 6, side = "left", pad = "0")) %>% 
  mutate(., minute_mod = str_pad(ResultMinute, width = 2, side = "left", pad = "0")) %>% 
  mutate(., hour_min_start = str_sub(rec_hour_mod, start = 1, end = 2)) %>% 
  mutate(., time_rec = paste(hour_min_start, minute_mod, sep = "")) %>% 
  mutate(., time_rec_charac = str_pad(time_rec, width = 6, side = "right", pad = "0")) %>% 
  mutate(., time_rec_numer = as.integer(time_rec_charac))

df_clustering <- select(df, X.1, BackgroundNoise, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, time_rec_numer, Date)
df_clustering <- df_clustering[order(df_clustering$time_rec_numer),]
df_clustering_test <- filter(df_clustering, Date == "20191015") %>% 
  select(X.1, AcousticComplexity, Ndsi, time_rec_numer)

test_data <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "mock_data_univar.csv"))

dtw <- dtw_basic(test_data, window.size = NULL, norm = "L2")
clustering1 <- tsclust(series = test_data, distance = "dtw_basic", type = "partitional", k = 2)
plot(clustering1)

vignette("dtwclust")

clustering1
clustering2
