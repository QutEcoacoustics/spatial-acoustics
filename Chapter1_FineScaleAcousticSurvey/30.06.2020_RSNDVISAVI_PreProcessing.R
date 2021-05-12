library(tidyverse)
library(purrr)
library(MuMIn)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
  
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
  
}

files <- list.files(path = getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted"), pattern = "NDVI.csv", recursive = T, full.names = T)

df <- lapply(files, read.csv) %>% 
  map_df(., select, c("NDVI_AVERAGE", "ID", "DATE")) %>% 
  write.csv(., getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted", "01.07.2020_AVGNDVI.csv"))


ndvi <- read.csv(getDataPath(chapter, "RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted", "26.02.2021_ndviavg.csv")) %>% 
  select(., Point, aug_ndvi_avg)
savi <- read.csv(getDataPath(chapter, "RemoteSensing", "2_SentinelHub_Landsat_SAVIExtracted", "26.02.2021_saviavg.csv"))%>% 
  select(., Point, aug_savi_avg)

df <- read.csv(getDataPath("Fieldwork_Bowra", "27.01.2021_data.csv"))

df1 <- right_join(x = savi, y = df)
df_filter <- filter(df1, DATE == "20191016")
df1 <- right_join(x = ndvi, y = df1)
df_filter <- select(df1, -c(X, X.x, X.2, X.y))
write.csv(df1, getDataPath("Fieldwork_Bowra", "26.02.2021_dataSAVINDVI.csv"), row.names = F)


df_filter1 <- select(df_filter, ID, everything())
df_filter1 <-  df_filter1 %>% mutate_at(vars(2:13), scale)
summary(df_filter1[,2:13])

correlation <- cor(df_filter1[,2:13])
write.csv(correlation, getDataPath("13.07.2020_cormatrix.csv"))
df_filter2 <- select(df_filter1, -c(SAVI_AVG, CanopyHeight))
write.csv(df_filter2, getDataPath("13.07.2020_newmodelvars.csv"))

glm <- glm(AcousticComplexity ~ CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH + NDVI_AVERAGE, data = df_filter1)

