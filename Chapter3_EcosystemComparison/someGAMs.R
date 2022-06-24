# Load the libraries 
library(mgcv)    # For GAMs
library(runjags) # For fitting a model in JAGs
library(raster)  # For simulating some spatial data
library(mvtnorm) # For simulating some spatial data.
library(tidyverse)
library(lubridate)

rm(list = ls())

set.seed(123)

set.group <- "bird"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("01.05.2022_fulldata.csv"))

data_og$date_r <- ymd(data_og$date_r)
data_og$day <- day(data_og$date_r)
data_og$week <- week(data_og$date_r)
data_og$month <- month(data_og$date_r)
data_og$year <- year(data_og$date_r)

data_og <- data_og %>% mutate(., bvg = case_when(bvg == 98 ~ as.integer(9),
                                                 TRUE ~ as.integer(bvg)))

data_og <- mutate(data_og, bvg_char = case_when(bvg == 2 ~ "tropical_rainforest",
                                                bvg == 4 ~ "euc_open_shruby_under",
                                                bvg == 8 ~ "euc_wood_shruby_under", 
                                                bvg == 9 ~ "euc_wood_grassy_under",
                                                bvg == 20 ~ "mulga_wood_grass_forbs",
                                                bvg == 31 ~ "saltbush_shrub",
                                                bvg == 62 ~ "dry_rainforest"))

data_og <- separate(data_og, col = ID.x, into = c("site", "point"), sep = "_", remove = F)
data_og <- filter(data_og, RFclass == set.group) %>% 
  droplevels()

site.coords <- as.matrix(unique(data_og[,c("lat","lon")]))
nsite <- 10



#Temporary GAM

tmp_jags <- mgcv::jagam(
  response ~ s(x,y, k = 9, bs = "ds", m = c(1,0.5)
  ),
  data = data.frame(response = rep(1, nsite),
                    x = site.coords[,1],
                    y = site.coords[,2]),
  family = "gaussian",
  file = "tmp1.jags"
)


covs <- group_by(data_og, lat, lon) %>% 
  select(., np_landscape_3k) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(., np_landscape_3k) %>% 
  mutate_at("np_landscape_3k", ~c(scale(.)))

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
                         "3" = "base::Super-Duper",
                         "4" = "base::Mersenne-Twister",
                         "5" = "base::Wichmann-Hill",
                         "6" = "base::Marsaglia-Multicarry",
                         "7" = "base::Super-Duper",
                         "8" = "base::Mersenne-Twister"
      ),
      .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain),
                "5" = gen_list(chain),
                "6" = gen_list(chain),
                "7" = gen_list(chain),
                "8" = gen_list(chain)
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
  burnin = 100000,
  sample = 20000,
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
