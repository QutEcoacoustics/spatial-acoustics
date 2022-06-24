library(tidyverse)
library(ggplot2)
# library(stringi)
# library(car)
# library(data.table)
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

rm(list = ls())

set.seed(123)

# set.group <- "bird"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, .drop = TRUE) {
  
  library(plyr)
  
  #New version of length which can handle NA'S if na.rm ==T, don't count them
  length2 <- function(x, na.rm = F) {
    if(na.rm) sum(!is.na(x))
    else length(x)
  }
  
  
  #This does the summary. For each group's dataframe, return a vector with N, mean and sd
  datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, col) {
    c(N = length2(xx[[col]], na.rm = na.rm),
      mean = mean(xx[[col]], na.rm = na.rm),
      sd = sd(xx[[col]], na.rm = na.rm)
    ) },
    measurevar
  )
  
  
  #Rename the "mean" column
  datac <- mutate(datac, measure_var = measurevar)
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd/sqrt(datac$N) #calculate sd of the mean
  
  #Confidence interval multiplier for sd
  #calculate t-stat for CI:
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

data_og <- read.csv(getDataPath("10.05.2022_fulldata_indclasses.csv")) %>% 
  dplyr::select(site, month, period, n, RFclass, everything(), -c(X, avg_rain_previous_months, avg_temp_previous_months, day, week)) %>%
  # mutate_at(c(6:7,10:32), ~(scale(.) %>% as.vector(.))) %>% 
  filter(site == "Eungella") %>% 
  group_by(month, RFclass, date_r, period) %>% 
  mutate(n = n()) %>% 
  distinct()

summary(data_og)

group_vars <- c("bvg_char")

data_og <- filter(data_og, RFclass == "bird") %>% 
  droplevels()

# soil_3k+ urban_3k+ water_3k+ cleared_3k+ natural_cover_3k+ contag_landscape_3k+ np_landscape_3k+ tca_landscape_3k+ soil_325+ urban_325+ water_325+ cleared_325+ natural_cover_325+ contag_landscape_325+ np_landscape_325+ tca_landscape_325+ ndvi_mean+ ebi_max+ ndwi_mean+ temp_max+ temp_min+ rain_value+ moon_illu + bvg
plot(data_og$ndvi_mean, data_og$n)
plot(data_og$temp_max, data_og$n)
plot(data_og$rain_value, data_og$n)

hist(data_og$n)

plot(data_og$moon_illu, data_og$n)

data_og$bvg <- as.factor(data_og$bvg)

m1 <- lme4::glmer(n ~ temp_max + natural_cover_325 + contag_landscape_325 + Recording_time + (1|period), family = "poisson", data = data_og)

vcov(m1)

summary(m1)

r.squaredGLMM(m1)

ranef(m1)
fixef(m1)

plot(m1)

coefplot::coefplot(m1)

options(na.action = na.fail)
modeltable <- dredge(m1)
mod2 <<- model.avg(get.models(modeltable, subset = T))
summary(mod2)


summarise_n <- summarySE(data_og, measurevar = "n", groupvars = group_vars)
summarise_ndvi_mean <- summarySE(data_og, measurevar = "ndvi_mean", groupvars = group_vars)
summarise_temp_max <- summarySE(data_og, measurevar = "temp_max", groupvars = group_vars)
summarise_rain_value <- summarySE(data_og, measurevar = "rain_value", groupvars = group_vars)

complete <- rbind(summarise_n, summarise_ndvi_mean, summarise_temp_max, summarise_rain_value)

complete %>% filter(measure_var != "n") %>% 
ggplot(., aes(x = site, y = mean, fill = measure_var)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-ci, ymax = mean+ci),
                width = .2,
                position = position_dodge(.9)) +
  confint.merMod(m1, level = 0.95, method = "boot", oldNames = F)
  REsim(m1)


plot <- plot_model(m1, type = "pred", terms = c("rain_value", "bvg"))
plot
plot[[1]]
plot[[2]]