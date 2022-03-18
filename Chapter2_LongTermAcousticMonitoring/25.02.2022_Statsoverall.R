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
  group_by(month) %>% 
  mutate(n_motif = n()) %>%
  mutate(avg_motif = paste("Motif/day=", format(round(n_motif/n_days, 0)))) %>% 
  mutate(n_motif_char = paste("n motifs=", n_motif, sep = "")) %>%
  mutate(n_days_char = paste("Recorded days=", n_days, sep = "")) %>%
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

library(cowplot)

# Hourly plots
plot_df <- plot_df[order(plot_df$month),] %>% 
  mutate(month_char = as.factor(case_when(month == "202001" ~ "January",
                           month == "202002" ~ "February",
                           month == "202003" ~ "March",
                           month == "202004" ~ "April",
                           month == "202005" ~ "May",
                           month == "202006" ~ "June",
                           month == "202007" ~ "July",
                           month == "202008" ~ "August",
                           month == "202009" ~ "September",
                           month == "202010" ~ "October",
                           month == "202011" ~ "November",
                           month == "202012" ~ "December")))

ggplot(data = plot_df, aes(x = as.factor(Recording_time), fill = RFclass)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Sound class", x = "Time", y = "Sound class count") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  theme_light(base_size = 13) +
  coord_polar() +
  facet_wrap(.~month_char + n_motif_char + n_days_char + avg_motif) +
  ggsave(getDataPath("Figures", "GoodFigs", "15.03.2022_RosePlot_hourly.jpg"), width = 16, height = 12)
  
#Anthrophony plot----

plot_df %>% filter(anthrophony == "yes") %>% 
  group_by(month) %>% 
  mutate(n_anthro = n()) %>%
  mutate(avg_motif_anthro = paste("Motif/day = ", format(round(n_anthro/n_days, 0)), sep = "")) %>% 
  mutate(n_anthro_char = paste("n motifs=", n_anthro, sep = "")) %>%
ggplot(data = ., aes(x = as.factor(Recording_time), fill = anthrophony)) + 
  geom_bar(aes(y = (..count..))) +
  #scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Anthrophony", x = "Time", y = "Anthrophony count") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  theme_light(base_size = 13) +
  theme(legend.position = "none")+
  coord_polar() +
  facet_wrap(.~month_char + n_anthro_char + n_days_char + avg_motif_anthro) +
  ggsave(getDataPath("Figures", "GoodFigs", "15.03.2022_anthrophony_RosePlot_hourly.jpg"), width = 14, height = 12)
  
plot_df %>% filter(anthrophony == "yes") %>% 
    group_by(month) %>% 
  mutate(n_anthro = n()) %>%
  mutate(avg_motif_anthro = paste("Motif/day = ", format(round(n_anthro/n_days, 0)), sep = "")) %>% 
  mutate(n_anthro_char = paste("n motifs=", n_anthro, sep = "")) %>%
    ggplot(data = ., aes(x = as.factor(week_day), fill = anthrophony)) + 
    geom_bar(aes(y = (..count..))) +
    #scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
    labs(fill = "Anthrophony", x = "Day of week", y = "Anthrophony count") +
    scale_x_discrete(labels = c("Monday" = "Mon", "Tuesday" = "Tue", "Wednesday" = "Wed", "Thursday" = "Thu", "Friday" = "Fri", "Saturday" = "Sat", "Sunday" = "Sun")) +
  theme_light(base_size = 13) +
  theme(legend.position = "none")+
    coord_polar() +
    facet_wrap(.~month_char + n_anthro_char + n_days_char + avg_motif_anthro) +
  ggsave(getDataPath("Figures", "GoodFigs", "15.03.2022_anthrophony_RosePlot_week.jpg"), width = 14, height = 12)

#Geophony plot ----

plot_df %>% filter(geophony == "yes") %>% 
  group_by(month) %>% 
  mutate(n_geo = n()) %>%
  mutate(avg_motif_geo = paste("Motif/day = ", format(round(n_geo/n_days, 2)), sep = "")) %>% 
  mutate(n_geo_char = paste("n motifs=", n_geo, sep = "")) %>%
  ggplot(data = ., aes(x = as.factor(Recording_time), fill = geophony)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("#756bb1")) +
  labs(fill = "Geophony", x = "Time", y = "Geophony count") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  theme_light(base_size = 13) +
  theme(legend.position = "none") +
  coord_polar() +
  facet_wrap(.~month_char + n_geo_char + n_days_char + avg_motif_geo) +
  ggsave(getDataPath("Figures", "GoodFigs", "15.03.2022_geophony_RosePlot_hourly.jpg"), width = 14, height = 12)



### Daily data----

data <- #filter(, period == period_test) %>% 
  select(plot_df, RFclass, period, general_category, Date, week_day, moon_illu, TempOut, HumOut, Rain, month, anthrophony, geophony, Recording_time, time2) %>% 
  na.exclude() %>% 
  droplevels()

n <- plot_df %>% group_by(month, RFclass) %>% 
  summarise(n = n()) %>% 
  write.csv(getDataPath("summary_classespermonth.csv"), row.names = F)

plot_df %>% filter(RFclass == "insect" | RFclass == "bird") %>% 
ggplot(data = ., aes(x = as.factor(month))) +
  geom_bar(aes(fill = RFclass)) +
  geom_violin(aes(y = NDVI_MEAN*1000, fill = "NDVI_MEAN")) +
  #geom_violin(aes(y = EBI_RANGE, fill = "EBI_RANGE")) +
  facet_wrap(.~RFclass)
  
  

### Daily model ----

rf <- randomForest(RFclass ~ period * moon_illu * TempOut * HumOut * Rain, data = data, importance = T, proximity = T)

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

rf_opt <- randomForest(RFclass ~ period * moon_illu * TempOut * HumOut * Rain, data = model_data, importance = T, proximity = T, mtry = best.m)

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
  ggsave(getDataPath("Figures", paste("25.02.2022_RosePlot_", ".jpg", sep = "")))


#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

data_month <- #filter(, period == period_test) %>%
  select(plot_df, period, RFclass, general_category, TempOut, HumOut, Rain, month, EBI_RANGE, NDVI_MEAN) %>% 
  na.exclude() %>% 
  droplevels() %>% 
  group_by(., month, RFclass) %>% 
  mutate(temp_mean = mean(TempOut),
         hum_mean = mean(HumOut),
         rain_mean = mean(Rain),
         ebi_mean = mean(EBI_RANGE),
         ndvi_mean = mean(NDVI_MEAN))

### Monthly model ----

rf_month <- randomForest(RFclass ~ period * temp_mean * hum_mean * rain_mean * ebi_mean * ndvi_mean, data = data_month, importance = T, proximity = T)

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

