rm(list = ls())

library(ggvegan)

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
  filter(n > 2) %>% 
  distinct() %>% 
  droplevels()

#Splitting the data

index <- createDataPartition(data_og$RFclass, p = .70, list = FALSE)
train <- data_og[index,]
test <- data_og[-index,]

m1 <- mlogit(RFclass ~ 0 | temp_max, data = data)
summary(m1)

#Best one so far ----
m1 <- multinom(RFclass ~ mean_temp + np_landscape_3k + contag_landscape_325 + moon_illu + bvg_char, data = train)

m1 <- multinom(RFclass ~ bvg_char + mean_temp + np_landscape_3k + contag_landscape_325 + moon_illu + natural_cover_3k + water_3k + tca_landscape_325 + ndvi_mean + rain_value, data = train)
summary(m1)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m1, m.null)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

#Best model ----
m2 <- multinom(RFclass ~ understory + mean_temp + moon_illu + natural_cover_3k + np_landscape_3k + tca_landscape_325
, data = train)
summary(m2)

m.null <- multinom(RFclass~1, data = train)
summary(m.null)

anova(m2, m.null)

options(na.action = na.fail)
modeltable <- dredge(m2)
mod2 <- model.avg(get.models(modeltable, subset = T))
summary(mod2)

z <- summary(m2)$coefficients/summary(m2)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(m2))

head(pp <- fitted(m2))

confint(m2, level = 0.95)

# Predicting the values for train dataset
train$ClassPredicted <- predict(m2, newdata = train, "class")
# Building classification table
tab <- table(train$RFclass, train$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

dataframe_landscape <- data_og %>%
  ungroup() %>% 
  dplyr::select(., ID.x, RFclass, site, bvg_char, veg_type,understory, temp_max, moon_illu, natural_cover_3k, np_landscape_3k, tca_landscape_325, ndvi_mean) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, site, bvg_char, ndvi_mean, temp_max, natural_cover_3k, np_landscape_3k, tca_landscape_325) %>%
  mutate(n = n(),
         natural_cover_3k = round(natural_cover_3k,2),
         tca_landscape_325 = round(tca_landscape_325,2),
         ndvi_mean = round(mean(ndvi_mean),2),
         np_landscape_3k = round(np_landscape_3k,2),
         temp_total = round(mean(temp_max), 2),
         moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, site, ID.x, bvg_char, everything(), -c(temp_max, moon_illu)) %>% 
  filter(n > 3) %>% 
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
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0

dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(6:11), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

sp_data <- decostand(dataframe_land_wide[,12:14], method = "hellinger")

rda <- vegan::rda(sp_data ~ understory + temp_total + tca_landscape_325, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)
summary(rda)
RsquareAdj(rda)

autoplot(rda)

ggplot(data = dataframe_landscape, aes(x = understory, fill = RFclass)) + 
  geom_bar(position = "dodge") 

ggplot(data = dataframe_landscape, aes(y = temp_total, x = RFclass)) +
  geom_boxplot()

ggplot(data = dataframe_landscape, aes(y = tca_landscape_325, x = RFclass)) +
  geom_boxplot()

ggplot(data = dataframe_landscape, aes(y = moon, x = RFclass)) +
  geom_boxplot()

ggord(rda, dataframe_land_wide$understory, vec_lab = c(temp_total = "mean_temp", moon = "moon", tca_landscape_325 = "total_core_area", understoryrainforest = "rainforest", understoryshrub = "shrub", understorytussock_grass = "grass", understorytussockgrass_forbs = "grass_forbs"), ptslab = TRUE, addsize = 3, xlim = c(-0.1, 0.2), ylim = c(-0.1,0.2)) + scale_fill_discrete(name = "Sites") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_general.jpg"))

#general - day/dawn

filtered <- filter(data_og, period == "day" | period == "dawn")

dataframe_landscape <- filtered %>%
  ungroup() %>% 
  dplyr::select(., ID.x, RFclass, site, bvg_char, temp_max, moon_illu, natural_cover_3k, np_landscape_3k, tca_landscape_325) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, site, bvg_char, temp_max, natural_cover_3k, np_landscape_3k, tca_landscape_325) %>%
  mutate(n = n(),
         natural_cover_3k = round(natural_cover_3k,2),
         tca_landscape_325 = round(tca_landscape_325,2),
         # water_3k = round(water_3k,2),
         np_landscape_3k = round(np_landscape_3k,2),
         temp_total = round(mean(temp_max), 2),
         moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, site, ID.x, bvg_char, everything(), -c(temp_max, moon_illu)) %>% 
  filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n")
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
# dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
# dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0

rda <- vegan::rda(dataframe_land_wide[,9] ~ bvg_char + temp_total + moon + natural_cover_3k + np_landscape_3k + tca_landscape_325, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

rda <- vegan::rda(dataframe_land_wide[,9] ~ bvg_char + temp_total, data = dataframe_land_wide)
anova(rda, by = "terms", permutations = 9999)

RsquareAdj(rda)
summary(rda)

autoplot(rda)


ggord(rda, dataframe_land_wide$site, vec_lab = c(natural_cover_3k = "natural_cover", temp_total = "mean_temperature", moon = "moon", bvg_chardry_rainforest = "dry_RF", bvg_chareuc_open_shruby_under = "EO_shruby_understory", bvg_chareuc_wood_grassy_under = "EW_grassy_understory", bvg_chareuc_wood_shruby_under = "EW_shruby_understory", bvg_charmulga_wood_grass_forbs = "MW_tussockgrass_forbs", bvg_charsaltbush_shrub = "SB_shrub", bvg_chartropical_rainforest = "subtropical_RF"), facet = T, ptslab = TRUE, addsize = 3, xlim = c(-1, 1)) + scale_fill_discrete(name = "Sites") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # looking at the raw code, this is plotting the 'wa scores', the blue dots are different species 
ggsave(getDataPath("Figures", "18.05.2022_rda_general.jpg"))
