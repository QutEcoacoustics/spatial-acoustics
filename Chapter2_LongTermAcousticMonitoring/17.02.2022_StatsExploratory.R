rm(list = ls())
library(tidyverse)
library(ggplot2)
library(randomForest)
library(rmarkdown)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment", ...))
}

df <- read.csv(getDataPath("15.02.2022_completedf.csv")) %>% 
  dplyr::select(RFclass, general_category, index_value, Date, week_day, Recording_time, month, index, moon_illu, TempOut, HumOut, Rain, EBI_RANGE, NDVI_MEAN, MSAVI_RANGE, everything(), -X) %>% 
  droplevels()

df$month <- as.factor(df$month)

# Climatic ----

df_monthly <- dplyr::select(df, RFclass, general_category, index_value, index, Date, week_day, Recording_time, month,  moon_illu, TempOut, HumOut, Rain) %>% 
  na.exclude(.) %>% 
  droplevels(.)

## General category; climatic vars + moon ----

rf_general_monthly <- randomForest(general_category ~ moon_illu + TempOut + HumOut + Rain + week_day + Recording_time + month, data = df_monthly, importance = T, proximity = T)

print(rf_general_climatic)

varImpPlot(rf_general_climatic)

nmds <- metaMDS(df_monthly[,9:12], k = 2)

### Optimising

importance <- as.data.frame(importance(rf_general)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- dplyr::select(df_monthly, general_category, all_of(importance)) %>%
  droplevels(.)



mtry <- tuneRF(model_data[-1],model_data$general_category, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


nmds <- metaMDS(df_monthly[,9:12], k = 2)


#RFclass; climatic vars + moon ----
                   
rf_class <- randomForest(RFclass ~ moon_illu + TempOut + HumOut + Rain + week_day + Recording_time + month, data = df_monthly)

print(rf)

varImpPlot(rf)

#Biophony only; climatic vars + moon ----

df_biophony <- dplyr::select(df, RFclass, general_category, index_value, index, Date, week_day, Recording_time, month,  moon_illu, TempOut, HumOut, Rain) %>% 
  filter(general_category == "biophony") %>% 
  na.exclude(.) %>% 
  droplevels(.)

rf_general <- randomForest(RFclass ~ moon_illu + TempOut + HumOut + Rain + week_day + month + Recording_time, data = df_biophony, importance = T, proximity = T)

print(rf_general)

varImpPlot(rf_general)

#Optimising

importance <- as.data.frame(importance(rf_general)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)
# 
model_data <- dplyr::select(df_biophony, RFclass, all_of(importance)) %>%
  droplevels(.)

#floor(sqrt(ncol(model_data) - 1))

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_general <- randomForest(RFclass ~ moon_illu + TempOut + HumOut + Rain + week_day + month + Recording_time, data = df_biophony, importance = T, proximity = T, mtry = mtry)

print(rf_general)

varImpPlot(rf_general)

nmds <- metaMDS(df_biophony[,9:11], k = 2)


nmds <- metaMDS(df_monthly[,9:12], k = 2)

#General category; satellite ----

df_monthly <- dplyr::select(df, RFclass, general_category, Date, week_day, Recording_time, month, NDVI_MEAN, EBI_RANGE, MSAVI_RANGE) %>% 
  na.exclude(.) %>% 
  droplevels(.)

rf_general <- randomForest(general_category ~ month + NDVI_MEAN + EBI_RANGE + MSAVI_RANGE, data = df_monthly)

print(rf_general)

varImpPlot(rf_general)


#BEST RESULT: RFclass; climatic vars + moon ----

df_monthly <- dplyr::select(df, RFclass, general_category, Date, week_day, Recording_time, month, NDVI_MEAN, EBI_RANGE, MSAVI_RANGE, TempOut) %>% 
  dplyr::filter(general_category == "biophony") %>% 
  na.exclude(.) %>% 
  droplevels(.)

rf <- randomForest(RFclass ~ month + NDVI_MEAN + EBI_RANGE + MSAVI_RANGE + Recording_time + TempOut, data = df_monthly, proximity = T, importance = T)

print(rf)

varImpPlot(rf)

#Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)
# 
model_data <- dplyr::select(df_monthly, RFclass, all_of(importance)) %>%
  droplevels(.)

#floor(sqrt(ncol(model_data) - 1))

mtry <- tuneRF(df_monthly[-1],df_monthly$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf <- randomForest(RFclass ~ month + NDVI_MEAN + EBI_RANGE + MSAVI_RANGE + Recording_time + TempOut, data = df_monthly, proximity = T, importance = T, mtry = 4)

print(rf)

varImpPlot(rf)


#BEST RESULT: general category; climatic vars + moon ----

df_monthly <- dplyr::select(df, RFclass, general_category, Date, week_day, Recording_time, month, NDVI_MEAN, EBI_RANGE, MSAVI_RANGE, TempOut, moon_illu, Rain, HumOut) %>% 
  dplyr::filter(general_category == "biophony" | general_category == "anthrophony" | general_category == "geophony") %>% 
  na.exclude(.) %>% 
  droplevels(.)

rf <- randomForest(general_category ~ Date + week_day + month + NDVI_MEAN + EBI_RANGE + MSAVI_RANGE + Recording_time + TempOut + moon_illu + Rain + HumOut, data = df_monthly, proximity = T, importance = T)

print(rf)

varImpPlot(rf)

#Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)
# 
model_data <- dplyr::select(df_monthly, general_category, all_of(importance)) %>%
  droplevels(.)

#floor(sqrt(ncol(model_data) - 1))

mtry <- tuneRF(df_monthly[-1],df_monthly$general_category, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf <- randomForest(general_category ~ ., data = model_data, proximity = T, importance = T, mtry = best.m)

print(rf)

varImpPlot(rf)

importance <- as.data.frame(importance(rf))