# Load the libraries 
library(mgcv)    # For GAMs
library(runjags) # For fitting a model in JAGs
library(raster)  # For simulating some spatial data
library(mvtnorm) # For simulating some spatial data.
library(tidyverse)
library(lubridate)
library(fda)
library(statip)
library(LearnBayes)

library(gamm4)
library(MuMIn)

rm(list = ls())

set.seed(123)

set.group <- "frog"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("09.05.2022_data.csv")) %>% 
  # mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  filter(RFclass == set.group) %>% 
  group_by(site, point, month, RFclass, period) %>% 
  mutate(n = n()) %>% 
  dplyr::select(everything(), -c(X, X.1, Recording_time, avg_rain_previous_months, future_ndvi, avg_temp_previous_months)) %>% 
  distinct()

summary(data_og)

# data_og <- read.csv(getDataPath("13.05.2022_fixingdata.csv"))
# 
# data_og$date_r <- ymd(data_og$date_r)
# data_og$day <- day(data_og$date_r)
# data_og$week <- week(data_og$date_r)
# data_og$month <- month(data_og$date_r)
# data_og$year <- year(data_og$date_r)
# 
# data_og <- data_og %>% mutate(., bvg = case_when(bvg == 98 ~ as.integer(9),
#                                                  TRUE ~ as.integer(bvg)))
# 
# data_og <- mutate(data_og, bvg_char = case_when(bvg == 2 ~ "tropical_rainforest",
#                                                 bvg == 4 ~ "euc_open_shruby_under",
#                                                 bvg == 8 ~ "euc_wood_shruby_under",
#                                                 bvg == 9 ~ "euc_wood_grassy_under",
#                                                 bvg == 20 ~ "mulga_wood_grass_forbs",
#                                                 bvg == 31 ~ "saltbush_shrub",
#                                                 bvg == 62 ~ "dry_rainforest"))
# 
# data_og <- separate(data_og, col = ID.x, into = c("site", "point"), sep = "_", remove = F) %>%
# # data_og <- filter(data_og, RFclass == set.group) %>%
#   rowwise() %>%
#   mutate(natural_cover_3k = sum(ca_class_4_3k, ca_class_8_3k, ca_class_3_3k, ca_class_9_3k, ca_class_6_3k)) %>%
#   mutate(natural_cover_325 = sum(ca_class_4_325, ca_class_8_325, ca_class_3_325, ca_class_9_325, ca_class_6_325)) %>%
#   ungroup() %>%
#   rename("urban_3k" = ca_class_5_3k) %>%
#   rename("urban_325" = ca_class_5_325) %>%
#   rename("water_3k" = ca_class_1_3k) %>%
#   rename("water_325" = ca_class_1_325) %>%
#   rename("cleared_3k" = ca_class_7_3k) %>%
#   rename("cleared_325" = ca_class_7_325) %>%
#   rename("soil_3k" = ca_class_2_3k) %>%
#   rename("soil_325" = ca_class_2_325)
# 
# write.csv(data_og, getDataPath("13.05.2022_fixingdata2.csv"))

#  # mutate(across(c(11:43,52,53), ~c(scale(.)))) %>%
#   droplevels() %>%
#   dplyr::select(date_r, Recording_time, ID.x, site, point, RFclass, lat, lon, n, soil_3k, urban_3k, water_3k, cleared_3k, natural_cover_3k, contag_landscape_3k, np_landscape_3k, tca_landscape_3k, soil_325, urban_325, water_325, cleared_325, natural_cover_325, contag_landscape_325, np_landscape_325, tca_landscape_325, ndvi_mean, ebi_max, ndwi_mean, temp_max, temp_min, rain_value, moon_illu, period, month_char, bvg, day, week, month, year, bvg_char)
# 
# data_og$point <- tolower(data_og$point)

# read.csv()
# 
# new_data <- left_join(data_og, df_ndvi, by = c("site", "point")) %>%
#   dplyr::select(everything(), -year.y)
# write.csv(data_og, getDataPath("10.05.2022_fulldata.csv"))
# site.coords <- as.matrix(unique(data_og[,c("lat","lon")]))
# nsite <- 10

# (n ~ soil_3k+ urban_3k+ water_3k+ cleared_3k+ natural_cover_3k+ contag_landscape_3k+ np_landscape_3k+ tca_landscape_3k+ soil_325+ urban_325+ water_325+ cleared_325+ natural_cover_325+ contag_landscape_325+ np_landscape_325+ tca_landscape_325+ ndvi_mean+ ebi_max+ ndwi_mean+ temp_max+ temp_min+ rain_value+ moon_illu + bvg + s(data_og$lat,data_og$lon, k = 9, bs = "ds", m = c(1,0.5)), random = ~(1|period) + (1|week), family = poisson(), data = data_og)

test <- gamm4(n ~ cleared_3k:natural_cover_325 + urban_325 + contag_landscape_3k + temp_max + rain_value + bvg, random = ~(1|period) + (1|site), family = poisson(), data = data_og)
plot(test$gam, pages = 2)
summary(test$gam) ## summary of gam
test$mer ## underlying mixed model
anova(test$gam)


test325 <- gamm4(n ~ urban_325+ water_325+ cleared_325+ natural_cover_325+ contag_landscape_325+ np_landscape_325+ tca_landscape_325, random = ~(1|period), family = poisson(), data = data_og)
plot(test325$gam, pages = 2)
summary(test325$gam) ## summary of gam
test325$mer ## underlying mixed model
anova(test325$gam)


test_climate <- gamm4(n ~ avg_temp_previous_years + bvg + past_ndvi + avg_rain_previous_years+ s(data_og$lat,data_og$lon, k = 9, bs = "ds", m = c(1,0.5)), random = ~(1|period), family = poisson(), data = data_og)
plot(test_climate$gam, pages = 2)
summary(test_climate$gam) ## summary of gam
test_climate$mer ## underlying mixed model
anova(test_climate$gam)


tmp_jags <- mgcv::jagam(
  response ~ s(x,y, k = 9, bs = "ds", m = c(1,0.5)
                                          ),
        data = data.frame(response = rep(1, nsite),
                          x = site.coords[,1],
                          y = site.coords[,2]),
        family = "binomial",
        file = "tmp1.jags"
  )


covs <- group_by(data_og, lat, lon) %>% 
  select(., np_landscape_3k) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(., np_landscape_3k) %>% 
  mutate_at("np_landscape_3k", ~c(scale(.))) %>% 
  rename("[,1]" = np_landscape_3k)

nsurvey <- 1


my_covars <- group_by(data_og, lat, lon) %>% 
  summarise(round(sum(n)*10/(sum(.$n)))) %>% 
  rename("y" = `round(sum(n) * 10/(sum(.$n)))`) %>% 
  distinct()

# data to include
data_list <- list(
  y = my_covars$y,
  X = tmp_jags$jags.data$X,
  S1 = tmp_jags$jags.data$S1,
  covs = as.matrix(covs[,1], ncol =1, nrow = nsite),
  nsite = nsite,
  nsurvey = nsurvey,
  zero = tmp_jags$jags.data$zero
) 


det_prob <- 0.5

# initial values function
my_inits <- function(chain){
  gen_list <- function(chain = chain){
    list(
      z = rep(1, nsite),
      beta_occ = rnorm(1),
      beta_det = rnorm(1),
      b = rnorm(
        length(tmp_jags$jags.ini$b),
        tmp_jags$jags.ini$b,
        0.2
      ),
      lambda = rgamma(1,1,1),
      .RNG.name = switch(chain,
                         "1" = "base::Wichmann-Hill",
                         "2" = "base::Wichmann-Hill",
                         "3" = "base::Super-Duper"
      ),
      .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain)
  )
  )
}

# Long model run to increase effective sample size
my_mod <- runjags::run.jags(
  model = "occupancy_gam.R",
  monitor = c("b", "beta_occ", "beta_det", "rho"),
  inits = my_inits,
  data = data_list,
  n.chains = 3,
  adapt = 1000,
  burnin = 4000,
  sample = 2000,
  thin = 20,
  method = "parallel"
)

# summarize model
my_sum <- summary(my_mod)


#proportion of 0's in the data
dat.tab<-table(my_covars$y==0)
dat.tab/sum(dat.tab)

FALSE 
1 
> 
  > #proportion of 0's expected from a Poisson distribution
mu <- mean(my_covars$y)
cnts <- rpois(1000, mu)
dat.tab <- table(cnts == 0)
dat.tab/sum(dat.tab)

FALSE  TRUE 
0.997 0.003 
