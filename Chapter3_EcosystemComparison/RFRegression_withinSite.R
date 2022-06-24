rm(list = ls())
library(ggord)
library(EnvStats)
library(randomForest)
library(tidyverse)

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(caret)
library(mlogit)
library(lubridate)

library(MuMIn)
library(vegan)
library(plotly)
library(processx)
library(lme4)
library(coefplot)
library(merTools)
library(sjPlot)
library(sjmisc)
library(report)
library(glmmTMB)


getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("13.05.2022_fixingdata5.csv")) %>% 
  # mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  # filter(RFclass == "bird" ) %>% 
  group_by(ID.x, RFclass, date_r) %>% 
  mutate(n = n()) %>% 
  # mutate(moon_illu = case_when(period =="day" ~ 0,
  #                              TRUE ~ moon_illu)) %>% 
  rowwise() %>% 
  mutate(., mean_temp = mean(c(temp_max,temp_min))) %>%
  dplyr::select(n, everything(), -c(Recording_time, day, week, id, id_path, fid_what, -ca_class_6_325)) %>% 
  mutate(., bvg = case_when(ID.x == "SERF_DryA" ~ as.numeric(9),
                            ID.x == "SERF_WetA" ~ as.numeric(2),
                            TRUE ~ as.numeric(bvg))) %>% 
  mutate(., bvg_char = case_when(bvg == 2 ~ "subtropical_rainforest",
                                 bvg == 9 ~ "euc_wood_grassy_under",
                                 TRUE ~ bvg_char)) %>% 
  mutate(., veg_type = case_when(bvg == 2 ~ "rainforest_vinethickets",
                                 bvg == 4 ~ "euc_open",
                                 bvg == 8 ~ "euc_wood", 
                                 bvg == 9 ~ "euc_wood",
                                 bvg == 20 ~ "acacia_wood",
                                 bvg == 31 ~ "shrubland",
                                 bvg == 62 ~ "rainforest_vinethickets")) %>% 
  mutate(., understory = case_when(bvg == 2 ~ "rainforest",
                                   bvg == 4 ~ "shrub",
                                   bvg == 8 ~ "shrub", 
                                   bvg == 9 ~ "tussock_grass",
                                   bvg == 20 ~ "tussockgrass_forbs",
                                   bvg == 31 ~ "shrub",
                                   bvg == 62 ~ "rainforest")) %>%
  mutate(ID = case_when(ID.x == "Booroopki_DryA" ~ "BRP_1",
                        ID.x == "Booroopki_WetA" ~ "BRP_2",
                        ID.x == "BonBon_DryA" ~ "BNB_1",
                        ID.x == "BonBon_WetA" ~ "BNB_2",
                        ID.x == "Bowra_DryA" ~ "BWR_1",
                        ID.x == "Bowra_WetA" ~ "BWR_2",
                        ID.x == "Eungella_DryA" ~ "ENG_1",
                        ID.x == "Eungella_WetA" ~ "ENG_2",
                        ID.x == "SERF_DryA" ~ "SRF_1",
                        ID.x == "SERF_WetA" ~ "SRF_2")) %>% 
  mutate(ID_new = case_when(ID.x == "Booroopki_DryA" ~ "Booroopki1",
                            ID.x == "Booroopki_WetA" ~ "Booroopki2",
                            ID.x == "BonBon_DryA" ~ "BonBon1",
                            ID.x == "BonBon_WetA" ~ "BonBon2",
                            ID.x == "Bowra_DryA" ~ "Bowra1",
                            ID.x == "Bowra_WetA" ~ "Bowra2",
                            ID.x == "Eungella_DryA" ~ "Eungella1",
                            ID.x == "Eungella_WetA" ~ "Eungella2",
                            ID.x == "SERF_DryA" ~ "SERF1",
                            ID.x == "SERF_WetA" ~ "SERF2")) %>% 
  filter(n > 2) %>% 
  distinct() %>% 
  droplevels()

ggplot(data_og, aes(x = ID_new, fill = RFclass)) +
  geom_bar(position = "dodge")


#Eungella1----

#bird ----

filtered <- filter(data_og, ID_new == "Eungella1", RFclass == "bird") %>% 
  ungroup() %>% 
  dplyr::select(n, moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(moon_illu = round(moon_illu, 2),
         ndvi_mean = round(ndvi_mean, 2),
         mean_temp = round(mean_temp, 1),
         rain_value = round(rain_value, 2)) %>% 
  group_by(moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(n = sum(n)) %>% 
  distinct()

# ggplot(data = filtered, aes(x = period)) +
#   geom_bar(position = "dodge") +
#   scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
#   # facet_wrap(.~period, scales = "free")
# ggsave(getDataPath("Figures", "eungella1_classperday.jpg"))

index <- createDataPartition(filtered$n, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = filtered, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance


test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

filtered$ClassPredicted <- predict(rf_opt, newdata = filtered, "class")
# Building classification table
tab <- table(filtered$n, round(filtered$ClassPredicted))

(a <- ggplot(filtered, aes(x = n, y = ClassPredicted)) +
  geom_point() +
  geom_smooth(colour = "black") +
  theme_bw() +
  annotate("text", label = "% Variation Explained = 25.29%", x = 8, y = 16))

(b <- ggplot(filtered, aes(x = n, y = mean_temp)) +
  geom_point() +
  geom_smooth(colour = "orange", span = 50) +
  theme_bw())

(c <- filter(filtered, ndvi_mean != 0) %>% 
  ggplot(., aes(x = n, y = ndvi_mean)) +
  geom_point() +
  geom_smooth(colour = "green", span = 50) +
  theme_bw())

(d <-
    ggplot(filtered, aes(x = n, y = rain_value)) +
    geom_point() +
    geom_smooth(colour = "blue", span = 50) +
    theme_bw())

cowplot::plot_grid(a, b, c, d, labels = c("A", "B", "C", "D"))
ggsave(getDataPath("Figures", "bird_eungella1.jpg"))

#insect ----

filtered <- filter(data_og, ID_new == "Eungella1", RFclass == "insect") %>% 
  ungroup() %>% 
  dplyr::select(n, moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(moon_illu = round(moon_illu, 2),
         ndvi_mean = round(ndvi_mean, 2),
         mean_temp = round(mean_temp, 1),
         rain_value = round(rain_value, 2)) %>% 
  group_by(moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(n = sum(n)) %>% 
  distinct()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "eungella1_insectsperday.jpg"))

index <- createDataPartition(filtered$n, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

filtered$ClassPredicted <- predict(rf_opt, newdata = filtered, "class")
# Building classification table
tab <- table(filtered$n, round(filtered$ClassPredicted))

plot(round(filtered$ClassPredicted), filtered$n)


(a <- ggplot(filtered, aes(x = n, y = ClassPredicted)) +
    geom_point() +
    geom_smooth(colour = "black") +
    theme_bw() +
    annotate("text", label = "% Variation Explained = 64.09%", x = 8, y = 16))

(b <- ggplot(filtered, aes(x = n, y = mean_temp)) +
    geom_point() +
    geom_smooth(colour = "orange", span = 50) +
    theme_bw())

(c <- filter(filtered, ndvi_mean != 0) %>% 
    ggplot(., aes(x = n, y = ndvi_mean)) +
    geom_point() +
    geom_smooth(colour = "green", span = 50) +
    theme_bw())

(d <-
    ggplot(filtered, aes(x = n, y = rain_value)) +
    geom_point() +
    geom_smooth(colour = "blue", span = 50) +
    theme_bw())

cowplot::plot_grid(a, b, c, d, labels = c("A", "B", "C", "D"))
ggsave(getDataPath("Figures", "bird_eungella1.jpg"))

#Eungella2----

#bird ----

filtered <- filter(data_og, ID_new == "Eungella2", RFclass == "bird") %>% 
  ungroup() %>% 
  dplyr::select(n, moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(moon_illu = round(moon_illu, 2),
         ndvi_mean = round(ndvi_mean, 2),
         mean_temp = round(mean_temp, 1),
         rain_value = round(rain_value, 2)) %>% 
  group_by(moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(n = sum(n)) %>% 
  distinct()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "eungella2_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)



#insect ----

filtered <- filter(data_og, ID_new == "Eungella2", RFclass == "insect") %>% 
  ungroup() %>% 
  dplyr::select(n, moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(moon_illu = round(moon_illu, 2),
         ndvi_mean = round(ndvi_mean, 2),
         mean_temp = round(mean_temp, 1),
         rain_value = round(rain_value, 2)) %>% 
  group_by(moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(n = sum(n)) %>% 
  distinct()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "eungella2_insectsperday.jpg"))

index <- createDataPartition(filtered$n, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

filtered$ClassPredicted <- predict(rf_opt, newdata = filtered, "class")
# Building classification table
tab <- table(filtered$n, round(filtered$ClassPredicted))

plot(round(filtered$ClassPredicted), filtered$n)

(a <- ggplot(filtered, aes(x = n, y = ClassPredicted)) +
    geom_point() +
    geom_smooth(colour = "black") +
    theme_bw() +
    annotate("text", label = "% Variation Explained = -10.77%", x = 8, y = 16))

(b <- ggplot(filtered, aes(x = n, y = mean_temp)) +
    geom_point() +
    geom_smooth(colour = "orange", span = 50) +
    theme_bw())

(c <- filter(filtered, ndvi_mean != 0) %>% 
    ggplot(., aes(x = n, y = ndvi_mean)) +
    geom_point() +
    geom_smooth(colour = "green", span = 50) +
    theme_bw())

(d <-
    ggplot(filtered, aes(x = n, y = rain_value)) +
    geom_point() +
    geom_smooth(colour = "blue", span = 50) +
    theme_bw())

cowplot::plot_grid(a, b, c, d, labels = c("A", "B", "C", "D"))
ggsave(getDataPath("Figures", "bird_eungella1.jpg"))

#Not enough samples to do frogs

#SERF1----

#bird ----

filtered <- filter(data_og, ID_new == "SERF1", RFclass == "bird") %>% 
  ungroup() %>% 
  dplyr::select(n, moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(moon_illu = round(moon_illu, 2),
         ndvi_mean = round(ndvi_mean, 2),
         mean_temp = round(mean_temp, 1),
         rain_value = round(rain_value, 2)) %>% 
  group_by(moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(n = sum(n)) %>% 
  distinct()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "serf1_birdperday.jpg"))

index <- createDataPartition(filtered$n, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

filtered$ClassPredicted <- predict(rf_opt, newdata = filtered, "class")
# Building classification table
tab <- table(filtered$n, round(filtered$ClassPredicted))

plot(round(filtered$ClassPredicted), filtered$n)

(a <- ggplot(filtered, aes(x = n, y = ClassPredicted)) +
    geom_point() +
    geom_smooth(colour = "black") +
    theme_bw() +
    annotate("text", label = "% Variation Explained = 17.51%", x = 15, y = 45))

(b <- ggplot(filtered, aes(x = n, y = mean_temp)) +
    geom_point() +
    geom_smooth(colour = "orange", span = 50) +
    theme_bw())

(c <- filter(filtered, ndvi_mean != 0) %>% 
    ggplot(., aes(x = n, y = ndvi_mean)) +
    geom_point() +
    geom_smooth(colour = "green", span = 50) +
    theme_bw())

(d <-
    ggplot(filtered, aes(x = n, y = rain_value)) +
    geom_point() +
    geom_smooth(colour = "blue", span = 50) +
    theme_bw())

(e <- 
    ggplot(filter, aes(x = n, y = moon_illu)) +
    geom_point() +
    geom_smooth(colour = "red", span = 50) +
    theme_bw())


cowplot::plot_grid(a, b, c, d, e, labels = c("A", "B", "C", "D", "E"))
ggsave(getDataPath("Figures", "bird_eungella1.jpg"))

#insect ----

filtered <- filter(data_og, ID_new == "SERF1", period == "insect") %>% 
  ungroup() %>% 
  dplyr::select(n, moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(moon_illu = round(moon_illu, 2),
         ndvi_mean = round(ndvi_mean, 2),
         mean_temp = round(mean_temp, 1),
         rain_value = round(rain_value, 2)) %>% 
  group_by(moon_illu, ndvi_mean, mean_temp, rain_value) %>% 
  mutate(n = sum(n)) %>% 
  distinct()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "serf1_insectsperday.jpg"))

index <- createDataPartition(filtered$n, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

filtered$ClassPredicted <- predict(rf_opt, newdata = filtered, "class")
# Building classification table
tab <- table(filtered$n, round(filtered$ClassPredicted))

plot(round(filtered$ClassPredicted), filtered $n)

(a <- ggplot(filtered, aes(x = n, y = ClassPredicted)) +
    geom_point() +
    geom_smooth(colour = "black") +
    theme_bw() +
    annotate("text", label = "% Variation Explained = 11.17%", x = 8, y = 16))

(b <- ggplot(filtered, aes(x = n, y = mean_temp)) +
    geom_point() +
    geom_smooth(colour = "orange", span = 50) +
    theme_bw())

(c <- filter(filtered, ndvi_mean != 0) %>% 
    ggplot(., aes(x = n, y = ndvi_mean)) +
    geom_point() +
    geom_smooth(colour = "green", span = 50) +
    theme_bw())

(d <-
    ggplot(filtered, aes(x = n, y = rain_value)) +
    geom_point() +
    geom_smooth(colour = "blue", span = 50) +
    theme_bw())

(e <-
    ggplot(filtered, aes(x = n, y = moon_illu)) +
    geom_point() +
    geom_smooth(colour = "red", span = 50) +
    theme_bw())

cowplot::plot_grid(a, b, c, d, e, labels = c("A", "B", "C", "D"))
ggsave(getDataPath("Figures", "bird_eungella1.jpg"))

#SERF2----

#bird ----

filtered <- filter(data_og, ID_new == "SERF2", RFclass == "bird") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "serf2_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#insect ----

filtered <- filter(data_og, ID_new == "SERF2", RFclass == "insect") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "serf2_insectsperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#BonBon1----

#bird ----

filtered <- filter(data_og, ID_new == "BonBon1", RFclass == "bird") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "bonbon1_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#insect ----

filtered <- filter(data_og, ID_new == "BonBon1", RFclass == "insect") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "bonbon1_insectsperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#frog ----

filtered <- filter(data_og, ID_new == "BonBon1", RFclass == "frog") %>% 
  ungroup()

#not enough samples (n = 11)

#BonBon2----

#Birds ----

filtered <- filter(data_og, ID_new == "BonBon2", RFclass == "bird") %>% 
  ungroup()

#not enough samples to analyse (n = 23)

#insect ----

filtered <- filter(data_og, ID_new == "BonBon2", RFclass == "insect") %>% 
  ungroup()

#not enough samples to analyse (n = 4)

#Booroopki1----

#bird ----

filtered <- filter(data_og, ID_new == "Booroopki1", RFclass == "bird") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "booroopki1_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#insect ----

filtered <- filter(data_og, ID_new == "Booroopki1", RFclass == "insect") %>% 
  ungroup()

#not enough samples (n = 3)


#Booroopki2----

#bird ----

filtered <- filter(data_og, ID_new == "Booroopki2", RFclass == "bird") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "booroopki2_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#insect ----

filtered <- filter(data_og, ID_new == "Booroopki2", RFclass == "insect") %>% 
  ungroup()

#not enough samples (n = 1)

#frog ----

filtered <- filter(data_og, ID_new == "Booroopki2", RFclass == "frog") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "booroopki2_frogperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)

#Bowra1----

#bird ----

filtered <- filter(data_og, ID_new == "Bowra1", RFclass == "bird") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "bowra1_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)


#insect ----

filtered <- filter(data_og, ID_new == "Bowra1", RFclass == "insect") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "bowra1_insectperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)


#Bowra2----

#bird ----

filtered <- filter(data_og, ID_new == "Bowra2", RFclass == "bird") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#c51b7d", "#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "bowra2_birdperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
tab <- table(test$n, round(test$ClassPredicted))

plot(round(test$ClassPredicted), test$n)


#insect ----

filtered <- filter(data_og, ID_new == "Bowra2", RFclass == "insect") %>% 
  ungroup()

ggplot(data = filtered, aes(x = period, fill = RFclass)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#5ab4ac"))
# facet_wrap(.~period, scales = "free")
ggsave(getDataPath("Figures", "bowra2_insectperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

rf <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = train, importance = T, proximity = T)

print(rf)

varImpPlot(rf)

#### Optimising

importance <- as.data.frame(importance(rf)) %>%
  filter(., `%IncMSE` >= 0) %>%
  row.names(.)

model_data <- dplyr::select(train, n, all_of(importance)) %>%
  droplevels(.)

mtry <- tuneRF(model_data[-1],model_data$n, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_opt <- randomForest(n ~ ndvi_mean + mean_temp + rain_value + moon_illu, data = model_data, importance = T, proximity = T, mtry = best.m)

print(rf_opt)

varImpPlot(rf_opt)

importance <- as.data.frame(importance(rf_opt))
importance

test$ClassPredicted <- predict(rf_opt, newdata = test, "class")
# Building classification table
(tab <- table(test$n, round(test$ClassPredicted)))

plot(round(test$ClassPredicted), test$n)
