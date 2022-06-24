library(gdm)
library(tidyverse)

rm(list = ls())

set.seed(123)

# set.group <- "bird"

#Splitting the data using a function from dplyr package



getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("13.05.2022_fixingdata5.csv")) %>% 
  # mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  # filter(RFclass == "bird" ) %>% 
  # mutate(moon_illu = case_when(period =="day" ~ 0,
  #                              TRUE ~ moon_illu)) %>% 
  rowwise() %>% 
  mutate(., mean_temp = mean(c(temp_max,temp_min))) %>%
  mutate(location = case_when(ID.x == "BonBon_WetA" ~ "7",
                              ID.x == "BonBon_DryA" ~ "6",
                              ID.x == "Booroopki_DryA" ~ "305",
                              ID.x == "Booroopki_WetA" ~ "306",
                              ID.x == "Bowra_DryA" ~ "259",
                              ID.x == "Bowra_WetA" ~ "258",
                              ID.x == "Eungella_DryA" ~ "110",
                              ID.x == "Eungella_WetA" ~ "111",
                              ID.x == "SERF_DryA" ~ "253",
                              ID.x == "SERF_WetA" ~ "254")) %>% 
  group_by(location, RFclass) %>% 
  mutate(n = n(),
         temp_total = mean(temp_max),
         rain_total = mean(rain_value),
         ndvi = mean(ndvi_mean)) %>% 
  dplyr::select(., RFclass, utm_lat, utm_lon, n, location, mean_temp, np_landscape_3k, contag_landscape_325, moon_illu, soil_3k, water_3k, tca_landscape_325, ndvi_mean, rain_value) %>% 
  # filter(n > 2) %>% 
  distinct()

# utm_coord <- read.csv(getDataPath("utm_sites_coord.csv"))
#   # dplyr::select(ID.x, utm_lat, utm_lon)
# 
# data_og <- full_join(data_og, utm_coord)

data_bio <- ungroup(data_og) %>%
  # group_by(ID.x, RFclass) %>% 
  # mutate(n = sum(n)) %>% 
  dplyr::select(., RFclass, utm_lat, utm_lon, n, location) %>% 
  # distinct() %>% 
  rename("sppCol" = RFclass,
         "y" = utm_lat,
         "x" = utm_lon,
         "abundance" = n) #,
         # "location" = ID.x)

data_bio$y <- format(round(data_bio$y, 8), nsmall = 8)
data_bio$x <- format(round(data_bio$x, 8), nsmall = 8)

data_bio$y <- as.numeric(data_bio$y)
data_bio$x <- as.numeric(data_bio$x)
data_bio$location <- as.factor(data_bio$location)
data_bio$sppCol <- as.factor(data_bio$sppCol)
data_bio$location <- as.numeric(data_bio$location)


format_bio <- 2

predictors <- ungroup(data_og) %>% 
  dplyr::select(., location, mean_temp, np_landscape_3k, contag_landscape_325, moon_illu, soil_3k, water_3k, tca_landscape_325, ndvi_mean, rain_value) %>% 
  mutate_at(c(2:10), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

# predictors$y <- format(round(predictors$y, 8), nsmall = 8)
# predictors$x <- format(round(predictors$x, 8), nsmall = 8)
# 
# predictors$y <- as.numeric(predictors$y)
# predictors$x <- as.numeric(predictors$x)
predictors$location <- as.factor(predictors$location)
predictors$location <- as.numeric(predictors$location)

sitepair <- formatsitepair(bioData = data_bio, bioFormat = format_bio, abundance = T, siteColumn = "location", XColumn = "x", YColumn = "y", sppColumn = "sppCol", abundColumn = "abundance", sppFilter = 2, predData = predictors)

gdm1 <- gdm(sitepair, geo = T)

summary(gdm1)

options(na.action = na.fail)
modeltable <- dredge(gdm1)
mod2 <<- model.avg(get.models(modeltable, subset = T))
summary(mod2)


gdm.crossvalidation(sitepair, train.proportion = 0.7, geo = T)
gdm.partition.deviance(sitePairTable = sitepair)

var_imp <- gdm.varImp(sitepair, geo = T)
plot(gdm1)
gdm.transform(gdm1, sitepair)
plotUncertainty(sitepair, bsIters = 1000, sampleSites = 0.7)
