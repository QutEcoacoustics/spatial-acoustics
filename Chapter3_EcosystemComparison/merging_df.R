#Merging complete motif and wavelet .csvs
library(tidyverse)

rm(list = ls())

#Functions ----
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/AIndices",  ...))
}

site <- "64"
point <- "253"
site_point <- paste(site, point, sep = "_")

files <- list.files(getDataPath("6_CompleteMotif"), pattern = site_point, full.names = T)

complete_df <- lapply(files, read.csv) %>% 
  bind_rows() %>% 
  group_by(id) %>% 
  filter(ResultMinute == min(ResultMinute)) %>% 
  distinct(id, .keep_all = T)

wavelet <- read.csv(getDataPath("8_FeatureExtraction", paste(site_point, "_202008_wavelet.csv", sep = "")))

merging <- merge(wavelet, complete_df, by = ) %>% 
  select(id, class, component, FileName, ResultMinute, length, date, time, position, everything()) %>% 
  rename(FileName_start = FileName) %>% 
  separate(time, into = c("time_real", "timezone_offset"), remove = F) %>%
  mutate(FileName_end = case_when(length + ResultMinute < 120 ~ as.character(FileName_start),
                                  length + ResultMinute == 120 ~ as.character(FileName_start),
                                  length + ResultMinute > 120 ~ case_when(as.numeric(time_real) == '220000' ~ paste((date+1), "T000000+0930_REC", sep = ""),
                                                                          #time == 110055 ~ paste(date, "_120100", sep = ""),
                                                                          as.integer(time_real) >= 080000 ~ paste(date, "T", as.integer((as.integer(time_real) + 20000)), "+0930_REC", sep = ""),
                                                                          as.integer(time_real) <= 080000 ~ paste(date, "T0", as.integer((as.integer(time_real) + 20000)), "+0930_REC", sep = "")))) %>% 
  select(point, position, index_value, ResultMinute, length, FileName_start, FileName_end, id, everything()) %>% 
  droplevels(.) %>% 
  mutate(FileName_start = paste("T:/Marina/a2o/", .$site, "/", .$point, "/", .$date, "_AAO", "/", .$FileName_start, ".flac", sep = "")) %>% 
  mutate(FileName_end = paste("T:/Marina/a2o/", .$site, "/", .$point, "/",  .$date, "_AAO", "/", .$FileName_end, ".flac", sep = "")) %>% 
  mutate(id_path = paste(getDataPath("7_CropSpectrogram"), "/", .$site, "/", .$point, "/", .$id, ".png", sep = "")) %>% 
  select(class, component, ResultMinute, length, FileName_start, FileName_end, id_path, id, everything()) %>% 
  write.csv(getDataPath("9_MergedDF", paste(site_point, "merged.csv", sep = "_")))
