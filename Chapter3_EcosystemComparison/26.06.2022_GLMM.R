library(tidyverse)
library(ggplot2)
library(markdown)
library(randomForest)
library(MuMIn)


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

df <- mutate(df, bvg = case_when(ID.x == "BonBon_DryA" ~ "31",
                              ID.x == "BonBon_WetA" ~ "31",
                              ID.x == "Booroopki_DryA" ~ "98",
                              ID.x == "Booroopki_WetA" ~ "8",
                              ID.x == "Bowra_DryA" ~ "20",
                              ID.x == "Bowra_WetA" ~ "9",
                              ID.x == "Eungella_DryA" ~ "4",
                              ID.x == "Eungella_WetA" ~ "62",
                              ID.x == "SERF_DryA" ~ "2",
                              ID.x == "SERF_WetA" ~ "98",
                              TRUE ~ "0")) %>% 
  select(everything(), -c(X2_3k, X5_3k, X9_3k, X16_3k, X60_3k, X98_3k, X2_325, X9_325, X98_325, X3_3k, X4_3k, X62_3k, X4_325, X62_325, X3_325, X20_3k, X21_3k, X31_3k, X20_325, X8_3k, X59_3k, X8_325, X59_325, X31_325, tca_class_1_325, tca_class_1_3k, tca_class_1_325, tca_class_2_3k, tca_class_2_325, tca_class_3_3k, tca_class_3_325, tca_class_4_3k, tca_class_4_325, tca_class_5_3k, tca_class_5_325, tca_class_6_3k, tca_class_6_325, tca_class_7_3k, tca_class_7_325, tca_class_8_3k, tca_class_8_325, tca_class_9_3k, tca_class_9_325, pr_landscape_325, pr_landscape_3k, np_class_1_3k, np_class_7_3k, np_class_8_3k, np_class_3_3k, np_class_9_3k, np_class_5_3k, np_class_6_3k, np_class_2_325, np_class_4_325, np_class_7_325, np_class_8_325, np_class_2_3k, np_class_4_3k, np_class_9_325, np_class_5_325, np_class_6_325))


library(glmmfields)
library(MuMIn)

data <- filter(df, RFclass != "NA" & new_month != "NA") %>% 
  group_by(date_r, Recording_time, ID.x, RFclass, lat, lon) %>% 
  summarise(n =  n()) %>%
  distinct() %>% 
  droplevels()


land_df <- select(df, ID.x, RFclass, ca_class_2_3k, ca_class_4_3k, contag_landscape_3k, np_landscape_3k, tca_landscape_3k, ca_class_1_3k, ca_class_7_3k, ca_class_8_3k, ca_class_3_3k, ca_class_9_3k, ca_class_5_3k, ca_class_6_3k, ca_class_2_325, ca_class_4_325, contag_landscape_325, np_landscape_325, tca_landscape_325, ca_class_7_325, ca_class_8_325, ca_class_1_325, ca_class_3_325, np_class_1_325, np_class_3_325, ca_class_9_325, ca_class_5_325, ca_class_6_325) %>% 
  group_by(ID.x, RFclass) %>%
  summarise_all(mean)
  

data2 <- full_join(data, land_df, by = c("ID.x", "RFclass"))

temp_df <- select(df, ID.x, RFclass, date_r, Recording_time, ndvi_mean, ebi_max, ndwi_mean, temp_max, temp_min, rain_value, moon_illu) %>% 
  group_by(ID.x, date_r, RFclass, Recording_time) %>%
  summarise_all(mean)

data2 <- inner_join(data2, temp_df, by = c("date_r", "Recording_time", "ID.x", "RFclass"))

categorical_df <- select(df, ID.x, RFclass, date_r, Recording_time, period, month_char, bvg) %>% 
  group_by(ID.x, RFclass, date_r, Recording_time) %>% 
  distinct()

data3 <- inner_join(data2, categorical_df, by = c("ID.x", "Recording_time", "date_r", "RFclass"))

data3[is.na(data3)] <- 0
data3 <- data.frame(data3)

write.csv(data3, getDataPath("01.05.2022_fulldata.csv"))

library(lme4)
library(coefplot)


cor.test(data3$ca_class_7_3k, data3$np_landscape_3k)
cor.test(data3$ca_class_7_3k, data3$ca_class_1_3k)

cor.test(data3$ca_class_7_3k, data3$ca_class_7_3k)
cor.test(data3$ca_class_7_3k, data3$ca_class_8_3k)
cor.test(data3$ca_class_7_3k, data3$ca_class_9_3k)


cor.test(data3$np_landscape_325, data3$np_landscape_325)
cor.test(data3$contag_landscape_325, data3$contag_landscape_325)
cor.test(data3$ca_class_1_325, data3$ca_class_1_325)
cor.test(data3$ca_class_2_325, data3$ca_class_2_325)
cor.test(data3$ca_class_3_325, data3$ca_class_3_325)
cor.test(data3$ca_class_4_325, data3$ca_class_4_325)
cor.test(data3$ca_class_5_325, data3$ca_class_5_325)
cor.test(data3$ca_class_7_325, data3$ca_class_7_325)
cor.test(data3$ca_class_8_325, data3$ca_class_8_325)
cor.test(data3$ca_class_9_325, data3$ca_class_9_325)

data4 <- data3 %>% as_tibble() %>% 
  mutate(across(8:40, ~c(scale(.)))) %>% 
  filter(period == "day") %>% 
  droplevels()

group_by(data4, RFclass) %>% 
  summarise(n())

data4 <- filter(data4, RFclass != "bat" & RFclass != "birdfroginsect" & RFclass != "birdmammal" & RFclass != "froginsectmammal")
summary(data4)
bird <- filter(data4, RFclass == "frog") %>% 
  droplevels()
bird$bvg <- as.factor(bird$bvg)

# np_landscape_3k + contag_landscape_325 + np_landscape_325 + ndvi_mean + ndwi_mean + ebi_max + temp_max + rain_value + moon_illu + (1|month_char) + (1|bvg)

model1_3k_bird <- glm(n ~  contag_landscape_325, family = "poisson", data = bird)

summary(model1_3k_bird)

r.squaredGLMM(model1_3k_bird)

ranef(model1_3k_bird)
fixef(model1_3k_bird)

plot(model1_3k_bird)

coefplot(model1_3k_bird)

bird$glmm_residuals <- residuals(model1_3k_bird)

plot(bird$ID.x, bird$glmm_residuals)

ggplot(data4, aes(ID.x, glmm_residuals, colour = glmm_residuals)) +
  geom_point()


geo <- as.matrix(dist(cbind(data4$lat, data4$lon)))
samples.dist.inv <- 1/geo
samples.dist.inv[1:5, 1:5]
diag(samples.dist.inv) <- 0
samples.dist.inv[is.infinite(samples.dist.inv)] <- 0


ape::Moran.I(data4$n, samples.dist.inv, alternative = "two.sided")

if(!require(ggregplot)) devtools::install_github("gfalbery/ggregplot", force = T)
# 
# library("devtools")
devtools::install_github(repo = "https://github.com/hrue/r-inla", ref = "stable", subdir = "rinla", build = FALSE, force = T)
# url::"https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz"
# 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
# update.packages(checkBuilt = TRUE)

# install.packages("bsblib", dependencies=TRUE)
# install.packages("shiny", dependencies = TRUE)
# install.packages("shiny", dependencies = TRUE)
library(INLA)

inla.upgrade(testing=TRUE)
library(ggplot2)
library(ggregplot)
library(tidyverse)
# library(RColorBrewer)
# library(brms)
# library(rstan)
library(glmmfields)

remove.packages(c("INLA"))
# 
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# model_1 <- brm(n ~ np_landscape_3k + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + moon_illu + (1|bvg), data = data4, family = poisson())
# 
# summary(model_1)
# plot(model_1, variable = c("b_np_landscape_3k", "b_"))

model1 <- inla(n ~ np_landscape_3k + ca_class_1_3k + ca_class_9_3k + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + bvg + moon_illu,"poisson", data4)

summary(model1)



model2 <- glmmfields(n ~ np_landscape_3k, nknots = 5, family = poisson(link = "log"), data = data4, lat = "lat", lon = "lon", chains = 1, iter = 500, control = list(max_treedepth = 20))

model2 <- glmmfields(n ~ np_landscape_3k + ca_class_1_3k + ca_class_9_3k + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + bvg + moon_illu, nknots = 5, family = poisson(link = "log"), data = data4, lat = "lat", lon = "lon", chains = 1, iter = 500, control = list(max_treedepth = 20))

print(model2)

plot(model2, type = "prediction") + scale_color_gradient2()
plot(model2, type = "spatial-residual")

# library(rstanarm)
# 
# model2_3k_day <- brm(n ~ np_landscape_3k + ca_class_1_3k + ca_class_9_3k + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + (1|bvg), family = "poisson",  data = data4, )

np_landscape_3k + ca_class_1_3k + ca_class_9_3k + ndvi_mean + ebi_max + ndwi_mean + temp_max + rain_value + bvg


mod2 <<- model.avg(get.models(modeltable, subset = T))
summary(mod2)

model1_325 <- glmer(n ~ ca_class_2_325 + ca_class_4_325 + contag_landscape_325 + np_landscape_325 + ca_class_7_325 + ca_class_8_325 + ca_class_1_325 + ca_class_3_325 + ca_class_9_325 + ca_class_5_325 + ndvi_mean + ebi_max + ndwi_mean + temp_max + temp_min + rain_value + moon_illu + period + (1|bvg), family = "poisson", data = data3)

summary(model1_325)

r.squaredGLMM(model1_325)
