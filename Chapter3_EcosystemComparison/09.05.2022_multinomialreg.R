library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(caret)
library(mlogit)
library(tidyverse)
library(report)

rm(list = ls())

set.seed(123)

# set.group <- "bird"

#Splitting the data using a function from dplyr package



getDataPath <- function (...) {
  return(file.path("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("13.05.2022_fixingdata5.csv")) %>% 
  # mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  # filter(RFclass == "bird" ) %>% 
  group_by(site, RFclass, date_r) %>% 
  mutate(n = n()) %>% 
  # mutate(moon_illu = case_when(period =="day" ~ 0,
  #                              TRUE ~ moon_illu)) %>% 
  rowwise() %>% 
  mutate(., mean_temp = mean(c(temp_max,temp_min))) %>%
  dplyr::select(n, everything(), -c(Recording_time, day, week, id, id_path, fid_what)) %>% 
  filter(n >= 2) %>% 
  distinct()

ggplot(data_og, aes(x = n)) + 
  geom_histogram() +
  facet_wrap(.~RFclass)
ggsave(getDataPath("Figures", "histogram_indclasses.jpg"))

data_og <- filter(data_og, RFclass == "bird" | RFclass == "frog" | RFclass == "insect") %>% 
  droplevels()

ggplot(data_og, aes(n)) + 
  geom_histogram() +
  facet_wrap(.~RFclass)
ggsave(getDataPath("Figures", "histogram_indclasses_afterfilter.jpg"))

data_og$bvg <- as.factor(data_og$bvg)

data <- mlogit.data(data_og, varying = NULL, choice = "RFclass", shape = "wide")



index <- caret::createDataPartition(data_og$RFclass, p = .70, list = FALSE)
train <- data_og[index,]
test <- data_og[-index,]



m1 <- mlogit(RFclass ~ 0 | temp_max + site + natural_cover_325 + period, data = data)
summary(m1)

m1 <- multinom(RFclass ~ temp_max + site + natural_cover_325 + period + bvg, data = train)

summary(m1)
report(m1)
m2 <- multinom(RFclass ~ site, data = train)
summary(m2)
m4 <- multinom(RFclass ~ natural_cover_325, data = train)
summary(m4)
m5 <- multinom(RFclass ~ moon_illu, data = train)
summary(m5)
m6 <- multinom(RFclass ~ period, data = train)
summary(m6)
m7 <- multinom(RFclass ~ avg_rain_previous_years, data = train)
summary(m7)
m8 <- multinom(RFclass ~ ndvi_mean, data = train)
summary(m8)
m9 <- multinom(RFclass ~ ebi_max, data = train)
summary(m9)
m10 <- multinom(RFclass ~ rain_value, data = train)
summary(m10)
m11 <- multinom(RFclass ~ point, data = train)
summary(m11)
m12 <- multinom(RFclass ~ contag_landscape_3k, data = train)
summary(m12)
m13 <- multinom(RFclass ~ bvg, data = train)
summary(m13)

z <- summary(m1)$coefficients/summary(m1)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(m1))

head(pp <- fitted(m1))

# Predicting the values for train dataset
train$ClassPredicted <- predict(m1, newdata = train, "class")
# Building classification table
tab <- table(train$RFclass, train$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

filter(data_og, site == "Booroopki") %>%
  droplevels() %>% 
  summary()

dEungella <- data.frame(site = rep("Eungella", 20), temp_max = sample(18:32, size = 20, replace = T), natural_cover_325 = sample(31:32, size = 20, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T), bvg = sample(c("62", "4"), size = 20, replace = T)) 

dBowra <- data.frame(site = rep("Bowra", 20), temp_max = sample(13:37, size = 20, replace = T), natural_cover_325 = sample(27:64, size = 20, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T), bvg = sample(c("20", "9"), size = 20, replace = T)) 

dBonBon <- data.frame(site = rep("BonBon", 20), temp_max = sample(10:39, size = 20, replace = T), natural_cover_325 = sample(0:2, size = 20, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T), bvg = sample(c("31"), size = 20, replace = T)) 

dBooroopki <- data.frame(site = rep("Booroopki", 20), temp_max = sample(9:28, size = 20, replace = T), natural_cover_325 = sample(17:31, size = 20, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T), bvg = sample(c("8", "9"), size = 20, replace = T)) 

dSERF <- data.frame(site = rep("SERF", 20), temp_max = sample(18:20, size = 20, replace = T), natural_cover_325 = sample(17:27, size = 20, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T), bvg = sample(c("2", "9"), size = 20, replace = T))

bvg_char <- data_og %>% ungroup() %>% dplyr::select(site, bvg, bvg_char) %>% 
  distinct()

bvg_char$bvg <- as.factor(bvg_char$bvg)

sim <- rbind(dEungella, dBonBon, dBowra, dBooroopki, dSERF) %>% 
  dplyr::left_join(., bvg_char, by = c("site", "bvg"))

sim$bvg <- as.factor(sim$bvg)
# dsite <- data.frame(site = sample(c("Eungella", "BonBon", "Booroopki", "Bowra", "SERF"), size = 20, replace = T), temp_max = sample(9:39, size = 20, replace = T), natural_cover_325 = sample(0.7:64, size = 20), moon_illu = sample(0:1, size = 20), period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T))
# predict(m7, newdata = dsite, "probs")

pp.site <- cbind(sim, predict(m1, newdata = sim, type = "probs", se = TRUE)) 

by(pp.site[, 7:9], pp.site$bvg_char, colMeans)

lpp <- melt(pp.site, id.vars = c("site", "period", "temp_max", "natural_cover_325", "bvg", "bvg_char"), value.name = "probability", variable.name = "group")
head(lpp)  # view first few rows


group_filter <- "bird"
filtered <- lpp %>% filter(group == group_filter)

ggplot(filtered, aes(x = temp_max, y = probability, colour = site)) +
  geom_smooth(se = F) +
  facet_grid(period ~ site) +
  theme_bw()
ggsave(getDataPath("Figures", paste("realsim_", group_filter, "_temp.jpg", sep = "")))


ggplot(filtered, aes(x = natural_cover_325, y = probability)) + geom_jitter() + geom_smooth(se = F) + facet_grid(group ~ site, scales = "free")
ggsave(getDataPath("Figures", paste("realsim_", group_filter, "natural.jpg", sep = "")))


ggplot(filtered, aes(x = site, y = probability, colour = bvg_char)) + geom_boxplot() + theme_bw()
ggsave(getDataPath("Figures", paste("realsim_", group_filter, "bvg.jpg", sep  = "")))

library(cowplot)

#Plots real data ----

group_filter <- "bird"
  filtered <- data_og %>% filter(RFclass == group_filter)

ggplot(filtered, aes(x = temp_max, colour = site)) + 
  geom_bar() + 
  facet_grid(RFclass ~ period ~ site) +
  theme_bw()
ggsave(getDataPath("Figures", paste("realdata_", group_filter, "_temp.jpg", sep = "")))

c1 <- ggplot(data_og, aes(x = natural_cover_325)) +
  geom_histogram() +
  theme_bw()

c2 <- ggplot(filtered, aes(x = natural_cover_325)) + 
  geom_bar() +
  # geom_smooth(se = F) + 
  facet_grid(site ~ RFclass) +
  theme_bw()

plot_grid(c1, c2)

ggsave(getDataPath("Figures", paste("realdata_", group_filter, "natural.jpg", sep = "")))

# lpp$site <- factor(lpp$site, levels = c("BonBon", "Eungella", "SERF", "Booroopki", "Bowra"))
ggplot(filtered, aes(x = site, fill = bvg_char)) +
  geom_bar(position = "dodge") +
  theme_bw()
ggsave(getDataPath("Figures", paste("realdata_", group_filter, "bvg.jpg", sep  = "")))

# Simulation with 1.5 increase in temp ----

set.seed(5)

sim_data <- filter(data_og, site == "Eungella") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dEungella <- data.frame(site = rep("Eungella", 50), temp_max = rnorm(n=50, mean = (mean+1.5), sd = sd), natural_cover_325 = sample(0:32, size = 50, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("62", "4"), size = 50, replace = T))

sim_data <- filter(data_og, site == "Bowra") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBowra <- data.frame(site = rep("Bowra", 50), temp_max = rnorm(n=50, mean = (mean+1.5), sd = sd), natural_cover_325 = sample(0:64, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("20", "9"), size = 50, replace = T)) 

sim_data <- filter(data_og, site == "BonBon") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBonBon <- data.frame(site = rep("BonBon", 50), temp_max = rnorm(n=50, mean = (mean+1.5), sd = sd), natural_cover_325 = sample(0:2, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("31"), size = 50, replace = T))

sim_data <- filter(data_og, site == "Booroopki") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBooroopki <- data.frame(site = rep("Booroopki", 50), temp_max = rnorm(n=50, mean = (mean+1.5), sd = sd), natural_cover_325 = sample(0:31, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("8", "9"), size = 50, replace = T)) 

sim_data <- filter(data_og, site == "SERF") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dSERF <- data.frame(site = rep("SERF", 50), temp_max =rnorm(n=50, mean = (mean+1.5), sd = sd), natural_cover_325 = sample(0:27, size = 50, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("2", "9"), size = 50, replace = T))

bvg_char <- data_og %>% ungroup() %>% dplyr::select(site, bvg, bvg_char) %>% 
  distinct()

sim <- rbind(dEungella, dBonBon, dBowra, dBooroopki, dSERF) %>% 
  dplyr::left_join(., bvg_char, by = c("site", "bvg"))

sim$bvg <- as.factor(sim$bvg)
# dsite <- data.frame(site = sample(c("Eungella", "BonBon", "Booroopki", "Bowra", "SERF"), size = 20, replace = T), temp_max = sample(9:39, size = 20, replace = T), natural_cover_325 = sample(0.7:64, size = 20), moon_illu = sample(0:1, size = 20), period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T))
# predict(m7, newdata = dsite, "probs")

pp.site <- cbind(sim, predict(m1, newdata = sim, type = "probs", se = TRUE)) 

by(pp.site[, 7:9], pp.site$bvg_char, colMeans)

lpp <- melt(pp.site, id.vars = c("site", "period", "temp_max", "natural_cover_325", "bvg", "bvg_char"), value.name = "probability", variable.name = "group")
head(lpp)  # view first few rows


group_filter <- "bird"
filtered <- lpp %>% filter(group == group_filter)
ggplot(filtered, aes(x = temp_max, y = probability, colour = site)) + geom_smooth(se = F) + facet_grid(period ~ site)
ggsave(getDataPath("Figures", paste("extremesim_", group_filter, "_temp.jpg", sep = "")))


ggplot(filtered, aes(x = natural_cover_325, y = probability)) + geom_jitter() + geom_smooth(se = F) + facet_grid(site ~ group)
ggsave(getDataPath("Figures", paste("extremesim_", group_filter, "natural.jpg", sep = "")))


ggplot(filtered, aes(x = site, y = probability, colour = bvg_char)) + geom_boxplot()
ggsave(getDataPath("Figures", paste("extremesim_", group_filter, "bvg.jpg", sep  = "")))


# Simulation with 2 increase in temp ----

set.seed(5)

sim_data <- filter(data_og, site == "Eungella") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dEungella <- data.frame(site = rep("Eungella", 50), temp_max = rnorm(n=50, mean = (mean+2), sd = sd), natural_cover_325 = sample(0:32, size = 50, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("62", "4"), size = 50, replace = T))

sim_data <- filter(data_og, site == "Bowra") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBowra <- data.frame(site = rep("Bowra", 50), temp_max = rnorm(n=50, mean = (mean+2), sd = sd), natural_cover_325 = sample(0:64, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("20", "9"), size = 50, replace = T)) 

sim_data <- filter(data_og, site == "BonBon") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBonBon <- data.frame(site = rep("BonBon", 50), temp_max = rnorm(n=50, mean = (mean+2), sd = sd), natural_cover_325 = sample(0:2, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("31"), size = 50, replace = T))

sim_data <- filter(data_og, site == "Booroopki") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBooroopki <- data.frame(site = rep("Booroopki", 50), temp_max = rnorm(n=50, mean = (mean+2), sd = sd), natural_cover_325 = sample(0:31, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("8", "9"), size = 50, replace = T)) 

sim_data <- filter(data_og, site == "SERF") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dSERF <- data.frame(site = rep("SERF", 50), temp_max =rnorm(n=50, mean = (mean+2), sd = sd), natural_cover_325 = sample(0:27, size = 50, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("2", "9"), size = 50, replace = T))

bvg_char <- data_og %>% ungroup() %>% dplyr::select(site, bvg, bvg_char) %>% 
  distinct()

sim <- rbind(dEungella, dBonBon, dBowra, dBooroopki, dSERF) %>% 
  dplyr::left_join(., bvg_char, by = c("site", "bvg"))

sim$bvg <- as.factor(sim$bvg)
# dsite <- data.frame(site = sample(c("Eungella", "BonBon", "Booroopki", "Bowra", "SERF"), size = 20, replace = T), temp_max = sample(9:39, size = 20, replace = T), natural_cover_325 = sample(0.7:64, size = 20), moon_illu = sample(0:1, size = 20), period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T))
# predict(m7, newdata = dsite, "probs")

pp.site <- cbind(sim, predict(m1, newdata = sim, type = "probs", se = TRUE)) 

by(pp.site[, 7:9], pp.site$site, colMeans)

lpp <- melt(pp.site, id.vars = c("site", "period", "temp_max", "natural_cover_325", "bvg", "bvg_char"), value.name = "probability", variable.name = "group")
head(lpp)  # view first few rows


group_filter <- "insect"
filtered <- lpp %>% filter(group == group_filter)
ggplot(filtered, aes(x = temp_max, y = probability, colour = site)) + geom_smooth(se = F) + facet_grid(period ~ site)
ggsave(getDataPath("Figures", paste("2c_sim_", group_filter, "_temp.jpg", sep = "")))


ggplot(filtered, aes(x = natural_cover_325, y = probability)) + geom_jitter() + geom_smooth(se = F) + facet_grid(site ~ group)
ggsave(getDataPath("Figures", paste("2c_sim_", group_filter, "natural.jpg", sep = "")))


ggplot(filtered, aes(x = site, y = probability, colour = bvg_char)) + geom_boxplot()
ggsave(getDataPath("Figures", paste("2c_sim_", group_filter, "bvg.jpg", sep  = "")))

# Simulation with 4 increase in temp ----

set.seed(5)

sim_data <- filter(data_og, site == "Eungella") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dEungella <- data.frame(site = rep("Eungella", 50), temp_max = rnorm(n=50, mean = (mean+4), sd = sd), natural_cover_325 = sample(0:32, size = 50, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("62", "4"), size = 50, replace = T))

sim_data <- filter(data_og, site == "Bowra") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBowra <- data.frame(site = rep("Bowra", 50), temp_max = rnorm(n=50, mean = (mean+4), sd = sd), natural_cover_325 = sample(0:64, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("20", "9"), size = 50, replace = T)) 

sim_data <- filter(data_og, site == "BonBon") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBonBon <- data.frame(site = rep("BonBon", 50), temp_max = rnorm(n=50, mean = (mean+4), sd = sd), natural_cover_325 = sample(0:2, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("31"), size = 50, replace = T))

sim_data <- filter(data_og, site == "Booroopki") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dBooroopki <- data.frame(site = rep("Booroopki", 50), temp_max = rnorm(n=50, mean = (mean+4), sd = sd), natural_cover_325 = sample(0:31, size = 50, replace = T),  period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("8", "9"), size = 50, replace = T)) 

sim_data <- filter(data_og, site == "SERF") %>%
  droplevels()

sd <- sd(sim_data$temp_max)
mean <- mean(sim_data$temp_max)

dSERF <- data.frame(site = rep("SERF", 50), temp_max =rnorm(n=50, mean = (mean+4), sd = sd), natural_cover_325 = sample(0:27, size = 50, replace = T), period = sample(c("dawn", "dusk", "day", "night"), size = 50, replace = T), bvg = sample(c("2", "9"), size = 50, replace = T))

bvg_char <- data_og %>% ungroup() %>% dplyr::select(site, bvg, bvg_char) %>% 
  distinct()

sim <- rbind(dEungella, dBonBon, dBowra, dBooroopki, dSERF) %>% 
  dplyr::left_join(., bvg_char, by = c("site", "bvg"))

sim$bvg <- as.factor(sim$bvg)
# dsite <- data.frame(site = sample(c("Eungella", "BonBon", "Booroopki", "Bowra", "SERF"), size = 20, replace = T), temp_max = sample(9:39, size = 20, replace = T), natural_cover_325 = sample(0.7:64, size = 20), moon_illu = sample(0:1, size = 20), period = sample(c("dawn", "dusk", "day", "night"), size = 20, replace = T))
# predict(m7, newdata = dsite, "probs")

pp.site <- cbind(sim, predict(m1, newdata = sim, type = "probs", se = TRUE)) 

by(pp.site[, 7:9], pp.site$site, colMeans)

lpp <- melt(pp.site, id.vars = c("site", "period", "temp_max", "natural_cover_325", "bvg", "bvg_char"), value.name = "probability", variable.name = "group")
head(lpp)  # view first few rows


group_filter <- "insect"
filtered <- lpp %>% filter(group == group_filter)
ggplot(filtered, aes(x = temp_max, y = probability, colour = site)) + geom_smooth(se = F) + facet_grid(period ~ site)
ggsave(getDataPath("Figures", paste("4c_sim_", group_filter, "_temp.jpg", sep = "")))


ggplot(filtered, aes(x = natural_cover_325, y = probability)) + geom_jitter() + geom_smooth(se = F) + facet_grid(site ~ group)
ggsave(getDataPath("Figures", paste("4c_sim_", group_filter, "natural.jpg", sep = "")))


ggplot(filtered, aes(x = site, y = probability, colour = bvg_char)) + geom_boxplot()
ggsave(getDataPath("Figures", paste("4c_sim_", group_filter, "bvg.jpg", sep  = "")))
