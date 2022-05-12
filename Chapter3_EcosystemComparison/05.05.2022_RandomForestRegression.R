# Load the libraries 
library(tidyverse)
library(lubridate)
library(randomForest)

rm(list = ls())

set.seed(123)

set.group <- "frog"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("01.05.2022_fulldata.csv"))

data_og$date_r <- ymd(data_og$date_r)
data_og$day <- day(data_og$date_r)
data_og$week <- week(data_og$date_r)
data_og$month <- month(data_og$date_r)
data_og$year <- year(data_og$date_r)

data_og <- data_og %>% mutate(., bvg = case_when(bvg == 98 ~ as.integer(9),
                                                 TRUE ~ as.integer(bvg)))

data_og <- mutate(data_og, bvg_char = case_when(bvg == 2 ~ "tropical_rainforest",
                                                bvg == 4 ~ "euc_open_shruby_under",
                                                bvg == 8 ~ "euc_wood_shruby_under", 
                                                bvg == 9 ~ "euc_wood_grassy_under",
                                                bvg == 20 ~ "mulga_wood_grass_forbs",
                                                bvg == 31 ~ "saltbush_shrub",
                                                bvg == 62 ~ "dry_rainforest"))

data_og <- separate(data_og, col = ID.x, into = c("site", "point"), sep = "_", remove = F)
data_og <- filter(data_og, RFclass == set.group) %>% 
  droplevels()

### Bird model ----

rf <- randomForest(n ~ np_landscape_3k + ca_class_2_325 + ca_class_4_325 + ca_class_3_325 + ca_class_1_325 + ca_class_5_325 + ca_class_7_325 + ca_class_8_325 + ca_class_9_325 + contag_landscape_325 + np_landscape_325 + ndvi_mean + ndwi_mean + ebi_max + temp_max + rain_value + moon_illu + month + bvg, data = data_og, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- select(data_og, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ np_landscape_3k + ca_class_2_325 + ca_class_4_325 + ca_class_3_325 + ca_class_1_325 + ca_class_5_325 + ca_class_7_325 + ca_class_8_325 + ca_class_9_325 + contag_landscape_325 + ndvi_mean + ndwi_mean + ebi_max + temp_max + rain_value + moon_illu + month + bvg, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance


