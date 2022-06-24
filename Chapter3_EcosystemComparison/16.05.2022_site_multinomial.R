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

hist(data_og)

ggplot(data_og, aes(x = n)) + 
  geom_histogram() +
  facet_wrap(.~RFclass)
ggsave(getDataPath("Figures", "14.05.2022_histogram_indclasses.jpg"))

hist(data_og)

ggplot(data_og, aes(n)) + 
  geom_histogram() +
  facet_wrap(.~RFclass)
ggsave(getDataPath("Figures", "14.05.2022_histogram_indclasses_afterfilter.jpg"))

data_og$bvg <- as.factor(data_og$bvg)

data <- mlogit.data(data_og, varying = NULL, choice = "RFclass", shape = "wide")

data_og$date_r <- ymd(data_og$date_r)

#Eungella----

filtered <- filter(data_og, site == "Eungella")

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
cor.test(data_og$natural_cover_325, data_og$soil_325) #keeping natural cover - positive corr
cor.test(data_og$np_landscape_325, data_og$tca_landscape_325) #keeping number of patches
cor.test(data_og$np_landscape_325, data_og$urban_325) #keeping number of pacthes

cor.test(data_og$natural_cover_325, data_og$np_landscape_325)
cor.test(data_og$np_landscape_325, data_og$mean_temp)
cor.test(data_og$mean_temp, data_og$moon_illu)
cor.test(data_og$moon_illu, data_og$water_325)
cor.test(data_og$water_325, data_og$contag_landscape_325)
cor.test(data_og$contag_landscape_325, data_og$cleared_325)
cor.test(data_og$cleared_325, data_og$ndvi_mean)
cor.test(data_og$ndvi_mean, data_og$rain_value)

m1 <- multinom(RFclass ~ bvg_char + period + natural_cover_325 + np_landscape_325 + mean_temp + moon_illu + water_325 + contag_landscape_325 + cleared_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ period + mean_temp , data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)


hist(filtered$water_325)

#Best one so far ----
linear <- lda(RFclass ~ period + mean_temp, data = train)
linear

p <- predict(linear, train)

par(mar=c(1,1,1,1))
ldahist(data = p$x, g = train$bvg_char)
ldahist(data = p$x[,2], g = train$bvg_char)

library(ggord)

ggord(linear, train$RFclass, ylim = c(-5, 5))

library(klaR)
library(psych)
library(MASS)

train$RFclass <- as.factor(train$RFclass)

partimat(RFclass ~ period + mean_temp, data = train, method = "lda")

p1 <- predict(linear, train)$class
# Building classification table
tab <- table(predicted = p1, actual = train$RFclass)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#SERF ----

filtered <- filter(data_og, site == "SERF")

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
# cor.test(data_og$natural_cover_325, data_og$soil_325) #keeping natural cover - positive corr
# cor.test(data_og$np_landscape_325, data_og$tca_landscape_325) #keeping number of patches
# cor.test(data_og$np_landscape_325, data_og$urban_325) #keeping number of pacthes
# 
# cor.test(data_og$natural_cover_325, data_og$np_landscape_325)
# cor.test(data_og$np_landscape_325, data_og$mean_temp)
# cor.test(data_og$mean_temp, data_og$moon_illu)
# cor.test(data_og$moon_illu, data_og$water_325)
# cor.test(data_og$water_325, data_og$contag_landscape_325)
# cor.test(data_og$contag_landscape_325, data_og$cleared_325)
# cor.test(data_og$cleared_325, data_og$ndvi_mean)
# cor.test(data_og$ndvi_mean, data_og$rain_value)

m1 <- multinom(RFclass ~ bvg_char + period + natural_cover_325 + np_landscape_325 + mean_temp + moon_illu + water_325 + contag_landscape_325 + cleared_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ period + mean_temp, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

#Bowra ----

filtered <- filter(data_og, site == "Bowra")

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
cor.test(data_og$natural_cover_325, data_og$soil_325) #keeping natural cover - positive corr
cor.test(data_og$np_landscape_325, data_og$tca_landscape_325) #keeping number of patches
cor.test(data_og$np_landscape_325, data_og$urban_325) #keeping number of pacthes

cor.test(data_og$natural_cover_325, data_og$np_landscape_325)
cor.test(data_og$np_landscape_325, data_og$mean_temp)
cor.test(data_og$mean_temp, data_og$moon_illu)
cor.test(data_og$moon_illu, data_og$water_325)
cor.test(data_og$water_325, data_og$contag_landscape_325)
cor.test(data_og$contag_landscape_325, data_og$cleared_325)
cor.test(data_og$cleared_325, data_og$ndvi_mean)
cor.test(data_og$ndvi_mean, data_og$rain_value)

m1 <- multinom(RFclass ~ bvg_char + period + natural_cover_325 + np_landscape_325 + mean_temp + moon_illu + water_325 + contag_landscape_325 + cleared_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ period + mean_temp + natural_cover_325 + ndvi_mean + rain_value, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

hist(filtered$water_325)

#BonBon ----

filtered <- filter(data_og, site == "BonBon")

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
# cor.test(data_og$natural_cover_325, data_og$soil_325) #keeping natural cover - positive corr
# cor.test(data_og$np_landscape_325, data_og$tca_landscape_325) #keeping number of patches
# cor.test(data_og$np_landscape_325, data_og$urban_325) #keeping number of pacthes
# 
# cor.test(data_og$natural_cover_325, data_og$np_landscape_325)
# cor.test(data_og$np_landscape_325, data_og$mean_temp)
# cor.test(data_og$mean_temp, data_og$moon_illu)
# cor.test(data_og$moon_illu, data_og$water_325)
# cor.test(data_og$water_325, data_og$contag_landscape_325)
# cor.test(data_og$contag_landscape_325, data_og$cleared_325)
# cor.test(data_og$cleared_325, data_og$ndvi_mean)
# cor.test(data_og$ndvi_mean, data_og$rain_value)

m1 <- multinom(RFclass ~ period + natural_cover_325 + np_landscape_325 + mean_temp + moon_illu + water_325 + contag_landscape_325 + cleared_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ np_landscape_325 + moon_illu + contag_landscape_325 + ndvi_mean + rain_value, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

hist(filtered$water_325)

#Booroopki ----

filtered <- filter(data_og, site == "Booroopki")

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
# cor.test(data_og$natural_cover_325, data_og$soil_325) #keeping natural cover - positive corr
# cor.test(data_og$np_landscape_325, data_og$tca_landscape_325) #keeping number of patches
# cor.test(data_og$np_landscape_325, data_og$urban_325) #keeping number of pacthes
# 
# cor.test(data_og$natural_cover_325, data_og$np_landscape_325)
# cor.test(data_og$np_landscape_325, data_og$mean_temp)
# cor.test(data_og$mean_temp, data_og$moon_illu)
# cor.test(data_og$moon_illu, data_og$water_325)
# cor.test(data_og$water_325, data_og$contag_landscape_325)
# cor.test(data_og$contag_landscape_325, data_og$cleared_325)
# cor.test(data_og$cleared_325, data_og$ndvi_mean)
# cor.test(data_og$ndvi_mean, data_og$rain_value)

m1 <- multinom(RFclass ~ period + natural_cover_325 + np_landscape_325 + mean_temp + moon_illu + water_325 + contag_landscape_325 + cleared_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

m2 <- multinom(RFclass ~ period + contag_landscape_325 + np_landscape_325 + rain_value, data = train)
summary(m2)

test$ClassPredicted <- predict(m2, newdata = test, "class")
# Building classification table
tab <- table(test$RFclass, test$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

hist(filtered$water_325)
