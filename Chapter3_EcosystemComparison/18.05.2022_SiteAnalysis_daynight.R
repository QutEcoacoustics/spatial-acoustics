rm(list = ls())
library(ggord)

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
  filter(n > 2) %>% 
  distinct() %>% 
  droplevels()


#Eungella----

#day+dawn ----

filtered <- filter(data_og, site == "Eungella", period == "day" | period == "dawn")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "eungella_classperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

#Correlations - test for filtered because they might not be on the general dataset but they are once you filter - it seems to happen a lot with land cover and landscape
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

#No moon on the day/dawn data

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ mean_temp, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char,  natural_cover_325, mean_temp, ndvi_mean, rain_value) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,7:9] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,7:9] ~ natural_cover_325 + mean_temp + ndvi_mean, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


library(ggord)
ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover", ndvi_mean = "NDVI", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1), ylim = c(-1.5, 1)) + scale_fill_discrete(name = "Vegetation groups", labels = c("dry rainforest", "eucalyptus open forest with shruby understory")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_eungella_day.jpg"))

#Eungella/night + dusk ----

filtered <- filter(data_og, site == "Eungella", period == "night" | period == "dusk")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "eungella_classpernight.jpg"))


index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ mean_temp + rain_value, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2),
         moon_illu = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0

rda <- vegan::rda(dataframe_land_wide[,8:10] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value+ moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,8:10] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value+ moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


library(ggord)
ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover", ndvi_mean = "NDVI", mean_temp = "mean_temperature", moon_illu = "moon_illumination", rain_value = "rain"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1)) + scale_fill_discrete(name = "Vegetation groups", labels = c("dry rainforest", "eucalyptus open forest with shruby understory")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_eungella_night.jpg"))


#SERF ----

filtered <- filter(data_og, site == "SERF")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "serf_classperperiod.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #keeping natural cover

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ bvg_char + natural_cover_325 + mean_temp + moon_illu + water_325 + contag_landscape_325 + cleared_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ moon_illu + mean_temp, data = train)
summary(m2)

options(na.action = na.fail)
modeltable <- dredge(m2)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)


test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----

dataframe_landscape <- filtered %>%  
  ungroup() %>% 
  dplyr::select(., RFclass, mean_temp, moon_illu) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, mean_temp, moon_illu) %>%
  mutate(n = n(), 
         # natural_cover_325 = round(natural_cover_325,2),
         # tca_landscape_325 = round(tca_landscape_325,2),
         # # water_3k = round(water_3k,2),
         # np_landscape_3k = round(np_landscape_3k,2),
         mean_temp = round(mean(mean_temp), 2), 
         moon_illu = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n")
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
# dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0

PERMANOVA <- adonis2(dataframe_land_wide[,3:4]~ dataframe_land_wide$mean_temp + dataframe_land_wide$moon_illu)
PERMANOVA

PERMANOVA2 <- adonis2(dataframe_land_wide[,6:7]~ dataframe_land_wide$period + dataframe_land_wide$mean_temp + dataframe_land_wide$natural_cover_325)
PERMANOVA2

rda <- vegan::rda(dataframe_land_wide[,3:4] ~ mean_temp + moon_illu, data = dataframe_land_wide)
RsquareAdj(rda)
summary(rda)
autoplot(rda)

m3 <- multinom(RFclass ~ period + mean_temp + natural_cover_325, data = train)
summary(m3)

test$ClassPredicted <- predict(m3, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#serf/day+dawn ----

filtered <- filter(data_og, site == "SERF", period == "day" | period == "dawn") %>% 
  droplevels

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "serf_classperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ ndvi_mean, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char,  natural_cover_325, mean_temp, ndvi_mean, rain_value) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
# dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,7:8] ~ mean_temp + natural_cover_325 + ndvi_mean + rain_value, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,7:8] ~ mean_temp + ndvi_mean, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(ndvi_mean = "NDVI", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-3, 3)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "sub tropical rainforest")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_serf_day.jpg"))

#serf/night + dusk ----

filtered <- filter(data_og, site == "SERF", period == "night" | period == "dusk")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "serf_classpernight.jpg"))


index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ moon_illu, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
# dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,8:9] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,8:9] ~ mean_temp + ndvi_mean + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)

sp <- dataframe_land_wide[,8:9]




ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(moon_illu = "moon_illumination", ndvi_mean = "NDVI", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-1, 1.5), ylim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "sub tropical rainforest")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_serf_night.jpg"))

#Bowra ----

filtered <- filter(data_og, site == "Bowra")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "bowra_classperperiod.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$water_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ bvg_char + period + natural_cover_325 + mean_temp + moon_illu + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

test$ClassPredicted <- predict(m1, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

m2 <- multinom(RFclass ~ period + mean_temp + natural_cover_325 + ndvi_mean + rain_value, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)


#bowra/day+dawn ----

filtered <- filter(data_og, site == "Bowra", period == "day" | period == "dawn") %>% 
  droplevels

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "bowra_classperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ ndvi_mean, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char,  natural_cover_325, mean_temp, ndvi_mean, rain_value) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
# dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,7:8] ~ mean_temp + natural_cover_325 + ndvi_mean + rain_value, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,7:8] ~ mean_temp + natural_cover_325, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "mulga")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_bowra_day.jpg"))

#bowra/night + dusk ----

filtered <- filter(data_og, site == "Bowra", period == "night" | period == "dusk")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "bowra_classpernight.jpg"))


index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)



#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2), 
         moon_illu = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,8:10] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,8:10] ~  natural_cover_325 + mean_temp + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)

ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(moon_illu = "moon_illumination", natural_cover_325 = "natural_cover", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1), ylim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "mulga")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_bowra_night.jpg"))

#BonBon ----

filtered <- filter(data_og, site == "BonBon")

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]


cor.test(filtered$natural_cover_325, filtered$soil_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ period + natural_cover_325 + mean_temp + moon_illu + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ period + natural_cover_325 + mean_temp, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA----

dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., ID.x, RFclass, bvg_char, period, natural_cover_325, mean_temp) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, bvg_char, period, natural_cover_325, mean_temp) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         # rain_value = round(mean(rain_value),3),
         # ndvi_mean = round(mean(ndvi_mean),3),
         # np_landscape_325 = round(np_landscape_325,2),
         mean_temp = round(mean(mean_temp), 3)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0

PERMANOVA <- adonis2(dataframe_land_wide[,6:8]~ dataframe_land_wide$natural_cover_325 + dataframe_land_wide$mean_temp + dataframe_land_wide$period)
PERMANOVA

PERMANOVA2 <- adonis2(dataframe_land_wide[,6:8]~ dataframe_land_wide$natural_cover_325 + dataframe_land_wide$period)
PERMANOVA2

rda <- vegan::rda(dataframe_land_wide[,6:8] ~ period + natural_cover_325, data = dataframe_land_wide)
RsquareAdj(rda)
summary(rda)
autoplot(rda)

m3 <- multinom(RFclass ~ period + natural_cover_325, data = train)
summary(m3)

test$ClassPredicted <- predict(m3, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#bonbon/day+dawn ----

filtered <- filter(data_og, site == "BonBon", period == "day" | period == "dawn") %>% 
  droplevels

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "bonbon_classperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~  natural_cover_325 + mean_temp, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char,  natural_cover_325, mean_temp, ndvi_mean, rain_value) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,7:9] ~ mean_temp + natural_cover_325 + ndvi_mean + rain_value, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,7:9] ~ natural_cover_325, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "mulga")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_bonbon_day.jpg"))

#BonBon/night + dusk ----

filtered <- filter(data_og, site == "BonBon", period == "night" | period == "dusk")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "bonbon_classpernight.jpg"))


index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ mean_temp + ndvi_mean, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)



#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2), 
         moon_illu = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,8:10] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,8:10] ~  natural_cover_325 + mean_temp, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)

ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1), ylim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("saltbush shrub")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_bonbon_night.jpg"))


#Booroopki ----

filtered <- filter(data_og, site == "Booroopki")

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]

cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #keeping natural cover

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ bvg_char + period + natural_cover_325 + mean_temp + moon_illu + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ period + natural_cover_325, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ---- 

dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., ID.x, RFclass, bvg_char, period, natural_cover_325) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, period, natural_cover_325) %>%
  mutate(n = n(), 
         # contag_landscape_325 = round(contag_landscape_325,2),
         # rain_value = round(mean(rain_value),3),
         # ndvi_mean = round(mean(ndvi_mean),3),
         natural_cover_325 = round(natural_cover_325,2)) %>% 
  # moon_illu = round(mean(moon_illu), 3)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


PERMANOVA <- adonis2(dataframe_land_wide[,5:7]~ dataframe_land_wide$period + dataframe_land_wide$natural_cover_325)
PERMANOVA

PERMANOVA2 <- adonis2(dataframe_land_wide[,5:7]~ dataframe_land_wide$natural_cover_325)
PERMANOVA2



rda <- vegan::rda(dataframe_land_wide[,5:7] ~ period + natural_cover_325, data = dataframe_land_wide)
summary(rda)
RsquareAdj(rda)

autoplot(rda)

m3 <- multinom(RFclass ~ natural_cover_325, data = train)
summary(m3)

test$ClassPredicted <- predict(m3, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)


#Booroopki/day+dawn ----

filtered <- filter(data_og, site == "Booroopki", period == "day" | period == "dawn") %>% 
  droplevels

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "booroopki_classperday.jpg"))

index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~  natural_cover_325 + mean_temp, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char,  natural_cover_325, mean_temp, ndvi_mean, rain_value) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,7:9] ~ mean_temp + natural_cover_325 + ndvi_mean + rain_value, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,7:9] ~ mean_temp + natural_cover_325, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover", mean_temp = "mean_temperature"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "eucalyptus woodland with shruby understory")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_booroopki_day.jpg"))

#BonBon/night + dusk ----

filtered <- filter(data_og, site == "Booroopki", period == "night" | period == "dusk")

ggplot(data = filtered, aes(x = RFclass, fill = RFclass)) +
  geom_bar() +
  scale_fill_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac")) +
  facet_wrap(.~period)
ggsave(getDataPath("Figures", "booroopki_classpernight.jpg"))


index <- createDataPartition(filtered$RFclass, p = .70, list = FALSE)
train <- filtered[index,]
test <- filtered[-index,]
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = mean_temp, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = soil_3k, colour = n))
# 
# ggplot(filtered) +
#   geom_jitter(aes(x = date_r, y = n, colour = natural_cover_3k, shape = site)) 
cor.test(filtered$natural_cover_325, filtered$soil_325) #soil is 0
cor.test(filtered$natural_cover_325, filtered$tca_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$urban_325) #urban is 0
cor.test(filtered$natural_cover_325, filtered$np_landscape_325) #removing np
cor.test(filtered$natural_cover_325, filtered$water_325) #water is 0
cor.test(filtered$natural_cover_325, filtered$contag_landscape_325) #keeping natural cover
cor.test(filtered$natural_cover_325, filtered$cleared_325) #cleared is 0

cor.test(filtered$natural_cover_325, filtered$mean_temp)
cor.test(filtered$natural_cover_325, filtered$moon_illu)
cor.test(filtered$natural_cover_325, filtered$ndvi_mean)
cor.test(filtered$natural_cover_325, filtered$rain_value)

m1 <- multinom(RFclass ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ natural_cover_325 + ndvi_mean, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)



#PERMANOVA ----
dataframe_landscape <- filtered %>% 
  ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, natural_cover_325, mean_temp, ndvi_mean, rain_value, moon_illu) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         # water_3k = round(water_3k,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         mean_temp = round(mean(mean_temp), 2), 
         moon_illu = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


rda <- vegan::rda(dataframe_land_wide[,8:10] ~ natural_cover_325 + mean_temp + ndvi_mean + rain_value + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,8:10] ~  natural_cover_325 + moon_illu, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)

ggord(rda, dataframe_land_wide$bvg_char, vec_lab = c(natural_cover_325 = "natural_cover", moon_illu = "moon_illumination"), ptslab = TRUE, addsize = 3, xlim = c(-1.5, 1), ylim = c(-1.5, 1.5)) + scale_fill_discrete(name = "Vegetation groups", labels = c("eucalyptus woodland with grassy understory", "eucalyptus woodland with shruby understory")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_booroopki_night.jpg"))
