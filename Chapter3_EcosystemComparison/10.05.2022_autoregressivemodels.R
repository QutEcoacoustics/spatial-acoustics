library(lubridate)

#Autoregressive models

rm(list = ls())

set.seed(123)

set.group <- "bird"

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("09.05.2022_data.csv")) %>% 
  mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  filter(RFclass == set.group) %>% 
  group_by(site, point, month, RFclass, period) %>% 
  mutate(n = n()) %>% 
  dplyr::select(everything(), -c(X, X.1, Recording_time, avg_rain_previous_months, future_ndvi, avg_temp_previous_months)) %>% 
  distinct()

data_og$date_r <- ymd(data_og$date_r)
data_og$day <- day(data_og$date_r)
data_og$week <- week(data_og$date_r)
data_og$month <- month(data_og$date_r)
data_og$year <- year(data_og$date_r)


ts <- ts(data_og[,10:33])
acf(ts)

ar <- ar.ols(data_og, order.max = 1, demean = F, intercept = T)
print(ar)
coeftest(ar)

ma <- arima(ts, order = c(0,0,1))
print(ma)
ts.plot(ts)
MA_fit <- ts - resid(ma)
points(MA_fit, type = "l", col = 2, lty = 2)
