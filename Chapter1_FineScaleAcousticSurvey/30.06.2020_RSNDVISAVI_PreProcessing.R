library(tidyverse)
library(purrr)
library(MuMIn)

getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey",  ...))
  
}


getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey",  ...))
  
}

files <- list.files(path = getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted"), pattern = ".csv", recursive = T, full.names = T)

df <- lapply(files, read.csv) %>% 
  map_df(., select, c("NDVI_AVERAGE", "ID", "DATE")) %>% 
  write.csv(., getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted", "01.07.2020_AVGNDVI.csv"))


ndvi <- read.csv(getDataPath("RemoteSensing", "2_SentinelHub_Landsat_NDVIExtracted", "13.07.2020_AVGNDVI_Oct.csv"))
savi <- read.csv(getDataPath("RemoteSensing", "2_SentinelHub_Landsat_SAVIExtracted", "13.07.2020_AVGSAVI_Oct.csv"))

df <- read.csv(getDataPath("08.06.2020_completedata.csv"))

df1 <- right_join(x = savi, y = df, by = c("ID" = "PointData"))
df_filter <- filter(df1, DATE == "20191016")
df1 <- right_join(x = ndvi, y = df1, by = c("ID"))
df_filter <- select(df1, -c(X, X.x, X.2, X.y))
write.csv(df_filter, getDataPath("13.07.2020_dataSAVINDVI.csv"))


df_filter1 <- select(df_filter, ID, everything())
df_filter1 <-  df_filter1 %>% mutate_at(vars(2:13), scale)
summary(df_filter1[2:13])

correlation <- cor(df_filter1[2:13])
write.csv(correlation, getDataPath("13.07.2020_cormatrix.csv"))
df_filter2 <- select(df_filter1, -c(SAVI_AVG, CanopyHeight))
write.csv(df_filter2, getDataPath("13.07.2020_newmodelvars.csv"))

glm <- glm(AcousticComplexity ~ CanopyCover + SubcanopyHeight + Slope + Aspect + Elevation + AVERAGE_NT_DIST + AVERAGE_NS_DIST + AVERAGE_GC_NG + AVERAGE_GC_NF + AVERAGE_GC_SH + NDVI_AVERAGE, data = df_filter1)

