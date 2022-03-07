library(tidyverse)
library(ggplot2)
library(markdown)
library(randomForest)


rm(list = ls())

set.seed(123)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.02.2022_completedf.csv"))

plot_df <- filter(df, general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony" | general_category == "biophony/geophony") %>% 
  mutate(., general_category = "biophony") %>% 
  mutate(., moon_illu = case_when(period == "day" ~ 0,
                               TRUE ~ moon_illu)) %>% 
  mutate(., RFclass = case_when(RFclass == "anthrobird" ~ "bird",
                                RFclass == "anthrobirdfroggeoinsect" ~ "birdfroginsect",
                                RFclass == "anthrobirdgeo" ~ "bird",
                                RFclass == "anthrobirdgeoinsect" ~ "birdinsect",
                                RFclass == "anthrobirdinsect" ~ "birdinsect",
                                RFclass == "anthrofroggeoinsect" ~ "froginsect",
                                RFclass == "anthrofroginsect" ~ "froginsect",
                                RFclass == "anthrogeoinsect" ~ "insect",
                                RFclass == "anthroinsect" ~ "insect",
                                RFclass == "birdgeo" ~ "bird",
                                RFclass == "birdgeoinsect" ~ "birdinsect",
                                RFclass == "froggeoinsect" ~ "froginsect",
                                RFclass == "geoinsect" ~ "insect",
                                TRUE ~ as.character(RFclass)
  )) %>% 
  droplevels(.)

plot_df$RFclass <- as.factor(plot_df$RFclass)
plot_df$period <- as.factor(plot_df$period)
plot_df$Date <- as.Date.character(plot_df$Date) %>% 
  sort()

plot_df$Recording_time <- factor(plot_df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

# Hourly plots

ggplot(data = plot_df, aes(x = as.factor(Recording_time), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Time", y = "% sound class") +
  coord_polar() +
  facet_wrap(.~month) +
  ggsave(getDataPath("Figures", "25.02.2022_RosePlot_hourly.jpg"), width = 15, height = 9)


period_test <- "day" 

### Daily data----

data <- filter(plot_df, period == period_test) %>% 
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2) %>% 
  na.exclude() %>% 
  droplevels()

### Daily model ----

rf <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain, data = data, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(data, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance


## Monthly data ----

### Rose plot - monthly biod
ggplot(data = data, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdinsect" = "#e9a3c9", "insect" = "#5ab4ac", "froginsect" = "#4d9221", "birdfroginsect" = "#9ebcda")) +
  labs(fill = "Sound class", x = "Month", y = "% sound class per period/month") +
  coord_polar() +
  ggsave(getDataPath("Figures", paste("25.02.2022_RosePlot_", period_test, ".jpg", sep = "")))


#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

data_month <- filter(plot_df, period == period_test) %>%
  select(., RFclass, general_category, TempOut, HumOut, Rain, month, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))

### Monthly model ----

rf_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * ebi_mean * ndvi_mean, data = data_month, importance = T, proximity = T)

print(rf_month)

varImpPlot(rf_month)

#### Optimising

importance <- as.data.frame(importance(rf_month)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(data_month, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_month_opt <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * ebi_mean * ndvi_mean, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_month_opt)

varImpPlot(rf_month_opt)

importance <- as.data.frame(importance(rf_month_opt))
importance

