library(tidyverse)
library(ggplot2)
library(markdown)
library(randomForest)


set.seed(123)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

df <- read.csv(getDataPath("24.02.2022_completedf.csv"))

plot_df <- filter(df, general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony" | general_category == "biophony/geophony") %>% 
  mutate(., general_category = "biophony") %>% 
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
  ggsave(getDataPath("Figures", "24.02.2022_RosePlot_hourly.jpg"), width = 15, height = 9)

## Dawn ----
### Daily data----

dawn <- filter(plot_df, period == "dawn") %>%
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2) %>% 
  na.exclude() %>% 
  droplevels()




### Daily model ----

rf_dawn <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain * anthrophony * geophony, data = dawn, importance = T, proximity = T)

print(rf_dawn)

varImpPlot(rf_dawn)

#### Optimising

importance <- as.data.frame(importance(rf_dawn)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(dawn, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dawn <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain * anthrophony * geophony, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dawn)

varImpPlot(rf_dawn)

importance <- as.data.frame(importance(rf_dawn))
importance

### Plots

library(ggthemes)
theme_set(theme_tufte())

importance

ggplot(data = dawn, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = TempOut/15, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dawn_temp.jpg"))

importance

dawn %>% filter(Recording_time != "2:00:00") %>% 
ggplot(data = ., aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#bdbdbd", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = HumOut/50, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#bdbdbd", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dawn_hum.jpg"))

importance

dawn %>%
  ggplot(data = ., aes(x = anthrophony, fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") +
  scale_fill_manual(values = c("#bdbdbd", "#e9a3c9", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dawndaily_anthrophony.jpg"))

## Monthly data ----

### Rose plot - monthly biod
ggplot(data = dawn, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Month", y = "% sound class per period/month") +
  coord_polar() +
  ggsave(getDataPath("Figures", "23.02.2022_RosePlot_dawn.jpg"))

dawn_month <- filter(plot_df, period == "dawn") %>%
  select(., RFclass, general_category, TempOut, HumOut, Rain, month, anthrophony, geophony, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass, anthrophony, geophony) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))

### Monthly model ----

rf_dawn_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = dawn_month, importance = T, proximity = T)

print(rf_dawn_month)

varImpPlot(rf_dawn_month)

#### Optimising

importance <- as.data.frame(importance(rf_dawn_month)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(dawn_month, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dawn_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dawn_month)

varImpPlot(rf_dawn_month)

importance <- as.data.frame(importance(rf_dawn_month))
importance

## Plots

importance

ggplot(data = dawn_month, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = temp_mean/10, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dawnmonth_temp.jpg"))

importance

ggplot(data = dawn_month, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = hum_mean/50, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dawnmonth_hum.jpg"))


#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda
  
  
## Day

# Day ----

day <- filter(plot_df, period == "day") %>%
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony) %>%
  na.exclude() %>%
  filter(., RFclass != "froginsect") %>% 
  droplevels()

## Day model ----

rf_day <- randomForest(RFclass ~ TempOut * HumOut * Rain * anthrophony * geophony, data = day, importance = T, proximity = T)

print(rf_day)

varImpPlot(rf_day)

### Optimising

importance <- as.data.frame(importance(rf_day)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(day, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_day <- randomForest(RFclass ~ TempOut * HumOut * Rain * anthrophony * geophony, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_day)

varImpPlot(rf_day)

importance <- as.data.frame(importance(rf_day))
importance

library(ggthemes)
theme_set(theme_tufte())

day %>%
ggplot(data = ., aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#bdbdbd", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = TempOut/20, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#bdbdbd", "#bdbdbd")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daydayly_temp.jpg"))

importance

day %>% 
  ggplot(data = ., aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#bdbdbd", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = HumOut/20, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#bdbdbd", "#bdbdbd")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daydayly_hum.jpg"))

importance

day %>% 
  ggplot(data = ., aes(x = anthrophony)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#bdbdbd", "#e9a3c9", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daydaily_anthrophony.jpg"))

#Monthly day data ----

day_month <- filter(plot_df, period == "day") %>%
  select(., RFclass, general_category, TempOut, HumOut, Rain, month, anthrophony, geophony, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass, anthrophony, geophony) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))

### Monthly model ----

rf_day_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = day_month, importance = T, proximity = T)

print(rf_day_month)

varImpPlot(rf_day_month)

#### Optimising

importance <- as.data.frame(importance(rf_day_month)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(day_month, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_day_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_day_month)

varImpPlot(rf_day_month)

importance <- as.data.frame(importance(rf_day_month))
importance


## Plots

ggplot(data = day_month, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = hum_mean/50, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#bdbdbd")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daymonth_hum.jpg"))

importance

ggplot(data = day_month, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#bdbdbd", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = ndvi_mean, colour = RFclass))+
  scale_color_manual(values = c("#252525", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daymonth_ndvi.jpg"))

importance

ggplot(data = day_month, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#bdbdbd", "#e9a3c9", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = ebi_mean*5, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daymonth_ebi.jpg"))

importance

ggplot(data = day_month, aes(x = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass), position = "dodge") +
  scale_fill_manual(values = c("#bdbdbd", "#bdbdbd", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_tufteboxplot(aes(y = temp_mean/10, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_daymonth_temp.jpg"))


#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect #9ebcda
#gray #bdbdbd



#Rose plot - monthly biod
ggplot(data = day, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Month", y = "% sound class per period/month") +
  coord_polar() +
  ggsave(getDataPath("Figures", "23.02.2022_RosePlot_day.jpg"))
## Dusk

# Plots----

dusk <- filter(plot_df, period == "dusk") %>%
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony,  Recording_time, time2) %>%
  na.exclude() %>% 
  droplevels()


#Rose plot - monthly biod
ggplot(data = dusk, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Month", y = "% sound class per period/month") +
  coord_polar() +
  ggsave(getDataPath("Figures", "23.02.2022_RosePlot_dusk.jpg"))


## Dusk model ----

rf_dusk <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain * anthrophony * geophony, data = dusk, importance = T, proximity = T)

print(rf_dusk)

varImpPlot(rf_dusk)

### Optimising

importance <- as.data.frame(importance(rf_dusk)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(dusk, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dusk <- randomForest(RFclass ~ moon_illu + TempOut * HumOut, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dusk)

varImpPlot(rf_dusk)

importance <- as.data.frame(importance(rf_dusk))
importance

## Plots ----

dusk %>% 
ggplot(data = ., aes(x = Date)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = RFclass)) +
  scale_fill_manual(values = c("#c51b7d", "#bdbdbd", "#bdbdbd")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_line(aes(y = TempOut/2000))+
  scale_x_date(date_breaks = "1 day") +
  #scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dusk_temp.jpg"))
  
importance

dusk %>% filter(RFclass != "bird") %>% 
  droplevels() %>% 
  ggplot(data = ., aes(x = sort(as.Date.factor(Date)))) + 
  geom_density(aes(y = (..count..)/sum(..count..), colour = RFclass), adjust = 1/12) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_density(aes(y = moon_illu/100), alpha = .2, stat = "identity") +
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  ggsave(getDataPath("Figures", "24.02.2022_dusk_moon.jpg"))

importance

dusk %>% filter(RFclass != "birdinsect") %>% 
  ggplot(data = ., aes(x = sort(as.Date.factor(Date)))) + 
  geom_density(aes(y = (..count..)/sum(..count..), colour = RFclass), adjust = 1/12) +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_line(aes(y = HumOut/15000), alpha = .5, stat = "identity")+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  facet_wrap(.~RFclass) +
  ggsave(getDataPath("Figures", "24.02.2022_duskdaily_hum.jpg"))

## Monthly data ---- 

dusk_month <- filter(plot_df, period == "dusk") %>%
  select(., RFclass, general_category, Date, TempOut, HumOut, Rain, month, anthrophony, geophony, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass, anthrophony, geophony) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))

### Monthly model ----

rf_dusk_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = dusk_month, importance = T, proximity = T)

print(rf_dusk_month)

varImpPlot(rf_dusk_month)

#### Optimising

importance <- as.data.frame(importance(rf_dusk_month)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(dusk_month, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dusk_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dusk_month)

varImpPlot(rf_dusk_month)

importance <- as.data.frame(importance(rf_dusk_month))
importance

ggplot(data = dusk_month, aes(x = sort(as.Date.character(Date)))) + 
  geom_density(aes(y = (..count..)/sum(..count..), colour = RFclass), adjust = 1/12) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_smooth(aes(y = temp_mean/200, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank())+
  facet_wrap(~RFclass) +
  ggsave(getDataPath("Figures", "24.02.2022_duskmonth_temp.jpg"))

importance


ggplot(data = dusk_month, aes(x = month)) + 
  geom_density(aes(y = (..count..)/sum(..count..), colour = RFclass), adjust = 1/12) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  labs(fill = "Sound class", x = "time", y = "% sound class per period") +
  geom_smooth(aes(y = ebi_mean/10, colour = RFclass))+
  scale_color_manual(values = c("#c51b7d", "#e9a3c9", "#5ab4ac")) +
  theme(axis.text.y = element_blank()) +
  facet_wrap(~RFclass) +
  ggsave(getDataPath("Figures", "24.02.2022_duskmonth_ebi.jpg"))


#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect #9ebcda
#gray #bdbdbd


## Night ----

# Daily plots----

night <- filter(plot_df, period == "night") %>%
  select(., RFclass, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony) %>%
  na.exclude() %>% 
  droplevels()


#Rose plot - monthly biod
ggplot(data = night, aes(x = as.factor(month), fill = RFclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Month", y = "% sound class per period/month") +
  coord_polar() +
  ggsave(getDataPath("Figures", "23.02.2022_RosePlot_night.jpg"))

## night model ----

rf_night <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain * anthrophony * geophony, data = night, importance = T, proximity = T)

print(rf_night)

varImpPlot(rf_night)

### Optimising

importance <- as.data.frame(importance(rf_night)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(night, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_night <- randomForest(RFclass ~ moon_illu + TempOut * HumOut * Rain * anthrophony * geophony, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_night)

varImpPlot(rf_night)

importance <- as.data.frame(importance(rf_night))
importance

## Monthly data ---- 

night_month <- filter(plot_df, period == "night") %>%
  select(., RFclass, general_category, TempOut, HumOut, Rain, month, anthrophony, geophony, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass, anthrophony, geophony) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))


### Monthly model ----

rf_night_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = night_month, importance = T, proximity = T)

print(rf_night_month)

varImpPlot(rf_night_month)

#### Optimising

importance <- as.data.frame(importance(rf_night_month)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(night_month, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_night_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * anthrophony * geophony * ebi_mean * ndvi_mean, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_night_month)

varImpPlot(rf_night_month)

importance <- as.data.frame(importance(rf_night_month))
importance
  
#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

#Overall model ----

overall <- select(plot_df, RFclass, period, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2) %>% 
  mutate(moon_illu = case_when(period == "day" ~ 0,
                               TRUE ~ moon_illu)) %>% 
  na.exclude() %>% 
  droplevels()

### Daily model ----

rf_overall <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain, data = overall, importance = T, proximity = T)

print(rf_overall)

varImpPlot(rf_overall)

#### Optimising

importance <- as.data.frame(importance(rf_overall)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(overall, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_overall <- randomForest(RFclass ~ moon_illu * TempOut * HumOut * Rain, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_overall)

varImpPlot(rf_overall)

importance <- as.data.frame(importance(rf_overall))
importance


# Monthly data ---- 
  
overall_month <- select(plot_df, RFclass, general_category, TempOut, HumOut, Rain, month, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))

### Monthly model ----

rf_overall_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean* ebi_mean * ndvi_mean, data = overall_month, importance = T, proximity = T)

print(rf_overall_month)

varImpPlot(rf_overall_month)

#### Optimising

importance <- as.data.frame(importance(rf_overall_month)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(overall_month, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_overall_month <- randomForest(RFclass ~ temp_mean * hum_mean * rain_mean * ebi_mean * ndvi_mean, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_overall_month)

varImpPlot(rf_overall_month)

importance <- as.data.frame(importance(rf_overall_month))
importance
