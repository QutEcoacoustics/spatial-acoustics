library(tidyverse)
library(ggplot2)
library(markdown)
library(randomForest)


rm(list = ls())

set.seed(123)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}


df <- read.csv(getDataPath("19.04.2022_df_final.csv")) %>% 
  mutate(technophony = case_when(general_category == "anthrophony" ~ "yes",
                                 general_category == "anthrophony/biophony" ~ "yes",
                                 general_category == "anthrophony/biophony/geophony" ~ "yes",
                                 general_category == "anthrophony/geophony" ~ "yes",
                                 TRUE ~ "no")) %>% 
  mutate(geophony = case_when(general_category == "anthrophony/biophony/geophony" ~ "yes",
                              general_category == "anthrophony/geophony" ~ "yes",
                              general_category == "biophony/geophony" ~ "yes",
                              general_category == "geophony" ~ "yes",
                              TRUE ~ "no"))



df <- filter(df, general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony" | general_category == "biophony/geophony") %>% 
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
                                RFclass == "anthrobatfroggeoinsect" ~ "batfroginsect",
                                RFclass == "anthrobirdfrog" ~ "birdfrog",
                                RFclass == "anthrobirdfroggeo" ~ "birdfrog",
                                RFclass == "anthrobirdfroginsect" ~ "birdfroginsect",
                                RFclass == "anthrobirdgeomammal" ~ "birdmammal",
                                RFclass == "anthrobirdmammal" ~ "birdmammal",
                                RFclass == "anthrofrog" ~ "frog",
                                RFclass == "anthrofroggeo" ~ "frog",
                                RFclass == "banjobird" ~ "birdfrog",
                                RFclass == "batbirdgeoinsect" ~ "batbirdinsect",
                                RFclass == "birdback" ~ "bird",
                                RFclass == "zerogeobird" ~ "bird",
                                RFclass == "birdfroggeo" ~ "birdfrog",
                                RFclass == "birdfroggeoinsect" ~ "birdfroginsect",
                                RFclass == "froggeo" ~ "frog",
                                RFclass == "geobat" ~ "bat",
                                RFclass == "insectfroggeo" ~ "froginsect",
                                RFclass == "insectgeo" ~ "insect",
                                RFclass == "zerobird" ~ "bird",
                                RFclass == "insectfrog" ~ "froginsect",
                                RFclass == "insectfrog" ~ "froginsect",
                                TRUE ~ as.character(RFclass)
  )) %>%
  droplevels() %>% 
  filter(RFclass != "NA") %>% 
  separate(., date_r, into = c("new_year", "new_month", "new_day"), sep = "-", remove = F) %>% 
  select(point, site, date_r, new_month, ID.x, RFclass, general_category, Recording_time,  everything(), -c(X, year, month, day))




  

#write.csv(plot_df, getDataPath("18.04.2022_dfcomplete_fixtime.csv"), row.names = F)

df$RFclass <- as.factor(df$RFclass)
df$period <- as.factor(df$period)
df$date_r <- as.Date.character(df$date_r)


df$new_month <- as.factor(df$new_month)

df$Recording_time <- factor(df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))
df$week_day <- factor(df$week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

df$technophony <- as.factor(df$technophony)
df$geophony <- as.factor(df$geophony)

df <- mutate(df, month_char = as.factor(case_when(new_month == "08" ~ "august",
                                                            new_month == "09" ~ "september",
                                                            new_month == "10" ~ "october")))

df$new_month <- factor(df$month_char, levels = c("august", "september", "october"))

summary(df)

#write.csv(plot_df, getDataPath("18.04.2022_df_filtered.csv"))

data <- filter(df, RFclass != "NA" & new_month != "NA") %>% 
  mutate(n_days = 92) %>% 
  select(RFclass, general_category, date_r, week_day, Recording_time, n_days, new_month, site, month_char, technophony, geophony) %>% 
  na.exclude() %>% 
  group_by(new_month, site) %>% 
  mutate(n_motif =  n()) %>%
  mutate(avg_motif = paste("Motif/day=", format(round(n_motif/n_days, 0)))) %>% 
  mutate(n_motif_char = paste("n motifs=", n_motif, sep = "")) %>%
  mutate(n_days_char = paste("Recorded days=", n_days, sep = "")) %>%
  mutate(month_nmotif_char = paste("N motif", month_char, "=", n_motif, sep = " ")) %>% 
  droplevels()

data$month_nmotif_char <- factor(data$month_nmotif_char, levels = c("N motif august = 974", "N motif september = 998", "N motif october = 1223", "N motif august = 1710", "N motif september = 1760", "N motif october = 1839", "N motif august = 1247", "N motif september = 1349", "N motif october = 1321", "N motif august = 261", "N motif september = 519", "N motif october = 585", "N motif august = 1389", "N motif september = 1416", "N motif october = 1696"))

labels <- select(data, new_month, n_motif, n_days, site) %>% 
  distinct() %>% 
  mutate(new_month = case_when(new_month == "08" ~ "Aug",
                               new_month == "09" ~ "Sep",
                               new_month == "10" ~ "Oct")) %>% 
  mutate(average_motif = paste("Motif/day = ", format(round(n_motif/n_days, 2)), sep = "")) %>% 
  mutate(labels = paste(new_month, average_motif, sep = "\n"))

data$site <- factor(data$site, levels = c("Eungella", "SERF", "Bowra", "BonBon", "Booroopki"))

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

# Hourly plots
data %>% filter(new_month != "NA") %>% 
  ggplot(data = ., aes(x = as.factor(Recording_time), fill = RFclass)) + 
  geom_bar() +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdfroginsect" = "#9ebcda", "birdinsect" = "#e9a3c9", "froginsect" = "#4d9221", "insect" = "#5ab4ac", "bat" = "#3690c0", "batbirdinsect" = "#c994c7", "batfroginsect" = "#3f007d", "birdfrog" = "#ef6548", "birdmammal" = "#a6bddb", "frog" = "#f7fcb9", "frogbird" = "#7bccc4", "froginsectmammal" = "#ccebc5")) +
  labs(fill = "Sound class", x = "Time", y = "% sound class") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  # coord_polar() +
  facet_wrap(~site + month_nmotif_char, nrow = 5, ncol = 3)
ggsave(getDataPath("Figures", "22.04.2022_BarPlot_hourly_month_site.jpg"), width = 15, height = 9)

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

library(cowplot)

#Anthrophony plot----

data %>% filter(technophony == "yes") %>%
  group_by(new_month, site) %>% 
  mutate(n_anthro = n()) %>%
  #mutate(avg_motif_anthro = paste("Motif/day = ", format(round(n_anthro/n_days, 0)), sep = "")) %>% 
  mutate(n_anthro_char = paste("n motifs", month_char, "=", n_anthro, sep = " ")) %>%
ggplot(data = ., aes(x = as.factor(Recording_time), fill = technophony)) + 
  geom_bar(aes(y = (..count..))) +
  #scale_fill_manual(values = c("#c51b7d", "#9ebcda", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  labs(fill = "Technophony", x = "Time", y = "Technophony count") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  theme_light(base_size = 13) +
  theme(legend.position = "none")+
  #coord_polar() +
  facet_wrap(.~site + n_anthro_char, ncol = 3, nrow = 5) 
  ggsave(getDataPath("Figures", "19.04.2022_anthrophony_hourly.jpg"), width = 14, height = 12)

#Geophony plot ----

data %>% filter(geophony == "yes") %>% 
  group_by(new_month, site) %>% 
  mutate(n_geo = n()) %>%
  #mutate(avg_motif_geo = paste("Motif/day = ", format(round(n_geo/n_days, 2)), sep = "")) %>% 
  mutate(n_geo_char = paste("n motifs", month_char, "=", n_geo, sep = " ")) %>%
  ggplot(data = ., aes(x = as.factor(Recording_time), fill = geophony)) + 
  geom_bar(aes(y = (..count..))) +
  scale_fill_manual(values = c("#756bb1")) +
  labs(fill = "Geophony", x = "Time", y = "Geophony count") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  theme_light(base_size = 13) +
  theme(legend.position = "none") +
  #coord_polar() +
  facet_wrap(.~site + n_geo_char, ncol = 3, nrow = 5)
  ggsave(getDataPath("Figures", "19.04.2022_geophony_hourly.jpg"), width = 14, height = 12)



### Phenology data----

phenology_all <- #filter(, period == period_test) %>% 
  select(df, RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, temp_min, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels()
  
cor.test(phenology_all$moon_illu, phenology_all$ndvi_mean)
cor.test(phenology_all$moon_illu, phenology_all$ebi_max)
cor.test(phenology_all$moon_illu, phenology_all$ndwi_mean)
cor.test(phenology_all$moon_illu, phenology_all$temp_max)
cor.test(phenology_all$moon_illu, phenology_all$temp_min)
cor.test(phenology_all$moon_illu, phenology_all$rain_value)

cor.test(phenology_all$ndvi_mean, phenology_all$ebi_max)
cor.test(phenology_all$ndvi_mean, phenology_all$ndwi_mean)
cor.test(phenology_all$ndvi_mean, phenology_all$temp_max)
cor.test(phenology_all$ndvi_mean, phenology_all$temp_min)
cor.test(phenology_all$ndvi_mean, phenology_all$rain_value)

cor.test(phenology_all$ebi_max, phenology_all$ndwi_mean)
cor.test(phenology_all$ebi_max, phenology_all$temp_max)
cor.test(phenology_all$ebi_max, phenology_all$temp_min)
cor.test(phenology_all$ebi_max, phenology_all$rain_value)

cor.test(phenology_all$ndwi_mean, phenology_all$temp_max)
cor.test(phenology_all$ndwi_mean, phenology_all$temp_min)
cor.test(phenology_all$ndwi_mean, phenology_all$rain_value)

cor.test(phenology_all$temp_max, phenology_all$temp_min)
cor.test(phenology_all$temp_max, phenology_all$rain_value)

cor.test(phenology_all$temp_min, phenology_all$rain_value)
  


n <- df %>% group_by(new_month, site, RFclass) %>% 
  summarise(n = n()) %>% 
  write.csv(getDataPath("19.04.2022_summary_classespermonth.csv"), row.names = F)

  
### overall model ----

rf <- randomForest(RFclass ~ period + moon_illu + week_day + site + new_month +  ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + technophony + geophony, data = phenology_all, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_all, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(RFclass ~ period + moon_illu + week_day + site + new_month +  ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + technophony + geophony, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance


## phenology day data ----

period_test <- "day"

phenology_day <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### Day model ----

rf_day <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_day, importance = T, proximity = T)

print(rf_day)

varImpPlot(rf_day)

#### Optimising

importance <- as.data.frame(importance(rf_day)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_day, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_day_opt <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_day_opt)

varImpPlot(rf_day_opt)

importance <- as.data.frame(importance(rf_day_opt))
importance


## phenology night data ----

period_test <- "night"

phenology_night <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### night model ----

rf_night <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_night, importance = T, proximity = T)

print(rf_night)

varImpPlot(rf_night)

#### Optimising

importance <- as.data.frame(importance(rf_night)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_night, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_day_opt <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_day_opt)

varImpPlot(rf_day_opt)

importance <- as.data.frame(importance(rf_day_opt))
importance

## phenology dusk data ----

period_test <- "dusk"

phenology_dusk <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### dusk model ----

rf_dusk <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_dusk, importance = T, proximity = T)

print(rf_dusk)

varImpPlot(rf_dusk)

#### Optimising

importance <- as.data.frame(importance(rf_dusk)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_dusk, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dusk_opt <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dusk_opt)

varImpPlot(rf_dusk_opt)

importance <- as.data.frame(importance(rf_dusk_opt))
importance

## phenology dawn data ----

period_test <- "dawn"

phenology_dawn <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### dawn model ----

rf_dawn <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_dawn, importance = T, proximity = T)

print(rf_dawn)

varImpPlot(rf_dawn)

#### Optimising

importance <- as.data.frame(importance(rf_dawn)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_dawn, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dawn_opt <- randomForest(RFclass ~ ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dawn_opt)

varImpPlot(rf_dawn_opt)

importance <- as.data.frame(importance(rf_dawn_opt))
importance


### Landscape data----

landscape_all <- #filter(, period == period_test) %>% 
  df[c(6,9:94)] %>%
  droplevels()

landscape_all[is.na(landscape_all)] <- 0

summary(landscape_all)
landscape_all_3k <- landscape_all[c(1,2:7,11:13,17:19,21,22,26:56)]

landscape_all_325 <- select(landscape_all, -colnames(landscape_all_3k))

cor(landscape_all_3k[,2:46], method = "spearman") %>% 
  write.csv(getDataPath("19.04.2022_landfinal3k_correlation.csv"))

cor.test(landscape_all$X2_3k, landscape_all$X5_3k)
cor.test(landscape_all$X5_3k, landscape_all$X3_3k)


cor.test(landscape_all$X9_3k, landscape_all$X60_3k)

cor.test(landscape_all$X2_3k, landscape_all$X98_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$X3_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$X4_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$X62_3k)
cor.test(landscape_all$ebi_max, landscape_all$X20_3k)
cor.test(landscape_all$ndwi_mean, landscape_all$X21_3k)
cor.test(landscape_all$ndwi_mean, landscape_all$X31_3k)
cor.test(landscape_all$temp_max, landscape_all$X8_3k)
cor.test(landscape_all$temp_max, landscape_all$X59_3k)


cor.test(landscape_all$moon_illu, landscape_all$X2_325)
cor.test(landscape_all$ndvi_mean, landscape_all$X9_325)
cor.test(landscape_all$ndvi_mean, landscape_all$X98_325)
cor.test(landscape_all$ebi_max, landscape_all$X4_325)
cor.test(landscape_all$ebi_max, landscape_all$X62_325)
cor.test(landscape_all$ebi_max, landscape_all$X3_325)
cor.test(landscape_all$ndwi_mean, landscape_all$X20_325)
cor.test(landscape_all$temp_min, landscape_all$X8_325)
cor.test(landscape_all$X2_3k, landscape_all$X59_325)
cor.test(landscape_all$X2_3k, landscape_all$X31_325)

cor.test(landscape_all$X2_3k, landscape_all$ca_class_2_3k)
cor.test(landscape_all$X2_3k, landscape_all$ca_class_4_3k)
cor.test(landscape_all$X2_3k, landscape_all$ np_class_2_3k)


cor.test(landscape_all$moon_illu, landscape_all$np_class_4_3k)

cor.test(landscape_all$ndvi_mean, landscape_all$ tca_class_2_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$tca_class_4_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$contag_landscape_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$np_landscape_3k)
cor.test(landscape_all$ndvi_mean, landscape_all$pr_landscape_3k)
cor.test(landscape_all$ebi_max, landscape_all$tca_landscape_3k)
cor.test(landscape_all$ebi_max, landscape_all$ca_class_1_3k)
cor.test(landscape_all$ebi_max, landscape_all$ca_class_7_3k)
cor.test(landscape_all$ebi_max, landscape_all$ca_class_8_3k)
cor.test(landscape_all$ebi_max, landscape_all$np_class_1_3k)
cor.test(landscape_all$ebi_max, landscape_all$np_class_7_3k)
cor.test(landscape_all$ebi_max, landscape_all$np_class_8_3k)
cor.test(landscape_all$ebi_max, landscape_all$tca_class_1_3k)


cor.test(landscape_all$ebi_max, landscape_all$ca_class_5_3k)

cor.test(landscape_all$ebi_max, landscape_all$tca_class_7_3k)
cor.test(landscape_all$ebi_max, landscape_all$tca_class_8_3k)
cor.test(landscape_all$ebi_max, landscape_all$ca_class_3_3k)

cor.test(landscape_all$ndwi_mean, landscape_all$np_class_3_3k)
cor.test(landscape_all$ndwi_mean, landscape_all$tca_class_3_3k)
cor.test(landscape_all$ndwi_mean, landscape_all$ca_class_9_3k      )

cor.test(landscape_all$temp_max, landscape_all$ np_class_9_3k   )
cor.test(landscape_all$temp_max, landscape_all$tca_class_9_3k)

cor.test(landscape_all$temp_min, landscape_all$ca_class_6_3k)
cor.test(landscape_all$ebi_max, landscape_all$np_class_5_3k)

cor.test(landscape_all$ebi_max, landscape_all$np_class_6_3k)
cor.test(landscape_all$ebi_max, landscape_all$tca_class_5_3k)
cor.test(landscape_all$ebi_max, landscape_all$tca_class_6_3k)

cor.test(landscape_all$ndwi_mean, landscape_all$ca_class_2_325)
cor.test(landscape_all$ndwi_mean, landscape_all$ca_class_4_325)
cor.test(landscape_all$ndwi_mean, landscape_all$np_class_2_325      )

cor.test(landscape_all$temp_max, landscape_all$ np_class_4_325   )
cor.test(landscape_all$temp_max, landscape_all$tca_class_2_325)

cor.test(landscape_all$temp_min, landscape_all$tca_class_4_325)

         contag_landscape_325 np_landscape_325 pr_landscape_325 tca_landscape_325 ca_class_7_325   ca_class_8_325   np_class_7_325  np_class_8_325   tca_class_7_325 tca_class_8_325  ca_class_1_325   ca_class_3_325 np_class_1_325   np_class_3_325   tca_class_1_325  tca_class_3_325   ca_class_9_325   np_class_9_325   tca_class_9_325  ca_class_5_325 ca_class_6_325   np_class_5_325 np_class_6_325  tca_class_5_325  tca_class_6_325
 
         cor.test(landscape_all$X2_3k, landscape_all$X5_3k)
         cor.test(landscape_all$X2_3k, landscape_all$X9_3k)
         cor.test(landscape_all$X2_3k, landscape_all$X16_3k)
         cor.test(landscape_all$X2_3k, landscape_all$X60_3k)
         cor.test(landscape_all$X2_3k, landscape_all$X98_3k)
         cor.test(landscape_all$ndvi_mean, landscape_all$X3_3k)
         cor.test(landscape_all$ndvi_mean, landscape_all$X4_3k)
         cor.test(landscape_all$ndvi_mean, landscape_all$X62_3k)
         cor.test(landscape_all$ebi_max, landscape_all$X20_3k)
         cor.test(landscape_all$ndwi_mean, landscape_all$X21_3k)
         cor.test(landscape_all$ndwi_mean, landscape_all$X31_3k)
         cor.test(landscape_all$temp_max, landscape_all$X8_3k)
         cor.test(landscape_all$temp_max, landscape_all$X59_3k)
         
         
         cor.test(landscape_all$moon_illu, landscape_all$X2_325)
         cor.test(landscape_all$ndvi_mean, landscape_all$X9_325)
         cor.test(landscape_all$ndvi_mean, landscape_all$X98_325)
         cor.test(landscape_all$ebi_max, landscape_all$X4_325)
         cor.test(landscape_all$ebi_max, landscape_all$X62_325)
         cor.test(landscape_all$ebi_max, landscape_all$X3_325)
         cor.test(landscape_all$ndwi_mean, landscape_all$X20_325)
         cor.test(landscape_all$temp_min, landscape_all$X8_325)
         cor.test(landscape_all$X2_3k, landscape_all$X59_325)
         cor.test(landscape_all$X2_3k, landscape_all$X31_325)
         
         cor.test(landscape_all$X2_3k, landscape_all$ca_class_2_3k)
         cor.test(landscape_all$X2_3k, landscape_all$ca_class_4_3k)
         cor.test(landscape_all$X2_3k, landscape_all$ np_class_2_3k)



n <- df %>% group_by(new_month, site, RFclass) %>% 
  summarise(n = n()) %>% 
  write.csv(getDataPath("19.04.2022_summary_classespermonth.csv"), row.names = F)


### overall model ----

rf <- randomForest(RFclass ~ period + moon_illu + week_day + site + new_month +  ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + technophony + geophony, data = phenology_all, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_all, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(RFclass ~ period + moon_illu + week_day + site + new_month +  ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + technophony + geophony, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance


## phenology day data ----

period_test <- "day"

phenology_day <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### Day model ----

rf_day <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_day, importance = T, proximity = T)

print(rf_day)

varImpPlot(rf_day)

#### Optimising

importance <- as.data.frame(importance(rf_day)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_day, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_day_opt <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_day_opt)

varImpPlot(rf_day_opt)

importance <- as.data.frame(importance(rf_day_opt))
importance


## phenology night data ----

period_test <- "night"

phenology_night <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### night model ----

rf_night <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_night, importance = T, proximity = T)

print(rf_night)

varImpPlot(rf_night)

#### Optimising

importance <- as.data.frame(importance(rf_night)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_night, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_day_opt <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_day_opt)

varImpPlot(rf_day_opt)

importance <- as.data.frame(importance(rf_day_opt))
importance

## phenology dusk data ----

period_test <- "dusk"

phenology_dusk <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### dusk model ----

rf_dusk <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_dusk, importance = T, proximity = T)

print(rf_dusk)

varImpPlot(rf_dusk)

#### Optimising

importance <- as.data.frame(importance(rf_dusk)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_dusk, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dusk_opt <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dusk_opt)

varImpPlot(rf_dusk_opt)

importance <- as.data.frame(importance(rf_dusk_opt))
importance

## phenology dawn data ----

period_test <- "dawn"

phenology_dawn <- filter(df, period == period_test) %>%
  select(., RFclass, period, general_category, date_r, week_day, moon_illu, site, new_month, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, rain_value, technophony, geophony) %>% 
  na.exclude() %>% 
  droplevels() 

### dawn model ----

rf_dawn <- randomForest(RFclass ~ moon_illu + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = phenology_dawn, importance = T, proximity = T)

print(rf_dawn)

varImpPlot(rf_dawn)

#### Optimising

importance <- as.data.frame(importance(rf_dawn)) %>%
  filter(., MeanDecreaseAccuracy >= 0) %>%
  row.names(.)

model_data <- select(phenology_dawn, RFclass, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-2],model_data$RFclass, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_dawn_opt <- randomForest(RFclass ~ ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_dawn_opt)

varImpPlot(rf_dawn_opt)

importance <- as.data.frame(importance(rf_dawn_opt))
importance


