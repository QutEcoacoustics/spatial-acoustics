library(tidyverse)
library(ggplot2)
library(lubridate)

getDataPath <- function (...) {
     return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
   }

# files <- list.files("D:/Chapter3_EcosystemComparison/historical_rain", pattern = "_temp_rain.csv", full.names = T)
# 
# df <- NULL
# 
# for (i in files) {
# x <- read.csv(i) %>% 
#   mutate(file_name = basename(i)) %>% 
#   dplyr::select(everything(), -c(metadata, latitude, longitude)) %>% 
#   separate(YYYY.MM.DD, into = c("year", "month", "day"), sep = "-", remove = F) %>% 
#   separate(file_name, into = c("site", "delete", "delete1"), sep = "_", remove = T) %>% 
#   dplyr::select(everything(), -c(delete, delete1)) %>% 
#   group_by(year, month, site) %>% 
#   mutate(mean_rain = mean(daily_rain),
#          mean_temp = mean(max_temp)) %>% 
#   dplyr::select(YYYY.MM.DD, year, month, day, mean_rain, mean_temp, site) %>% 
#   distinct()
# 
# x$YYYY.MM.DD <- as.Date(x$YYYY.MM.DD)
# 
# x %>% pivot_longer(., cols = c("mean_rain", "mean_temp"), names_to = "variable_name", values_to = "values") %>% 
# ggplot(., aes(YYYY.MM.DD, values)) +
#   geom_jitter(aes(colour = variable_name)) +
#   geom_smooth(aes(colour = variable_name)) +
#   theme_classic()
#  ggsave(getDataPath("Figures", gsub(x = basename(i), pattern = ".csv", replacement = ".jpg")))
# 
# write.csv(x, paste("D:/Chapter3_EcosystemComparison/historical_rain/summary_", basename(i), sep = ""), row.names = F)
# }
# 
# df <- NULL
# 
# files <- files[1:5]
# 
# 
# for (i in files) {
#   x <- read.csv(i) %>% 
#     mutate(file_name = basename(i)) %>% 
#     dplyr::select(everything(), -c(metadata, latitude, longitude)) %>% 
#     separate(YYYY.MM.DD, into = c("year", "month", "day"), sep = "-", remove = F) %>% 
#     separate(file_name, into = c("site", "delete", "delete1"), sep = "_", remove = T) %>% 
#     dplyr::select(everything(), -c(delete, delete1)) %>% 
#     group_by(year, site) %>% 
#     mutate(annual_rain = mean(daily_rain),
#            annual_temp = mean(max_temp)) %>% 
#     dplyr::select(YYYY.MM.DD, year, month, day, annual_rain, annual_temp, site) %>% 
#     distinct()
#   df <- rbind(df, x) 
# }
# 
# 
# df$year <- as.integer(df$year)
# 
# df1 <- dplyr::select(df, year, month, annual_rain, annual_temp, site) %>% 
#   distinct() %>% 
#   mutate("20year" = case_when(year >= 2000 & year <= 2019 ~ "previous_years",
#                               TRUE ~ "previous_months")) %>% 
#   group_by(`20year`, site) %>% 
#   mutate(avg_rain = mean(annual_rain),
#          avg_temp = mean(annual_temp)) %>% 
#   dplyr::select(site, avg_rain, avg_temp, `20year`) %>% 
#   distinct() %>% 
#   pivot_wider(., names_from = `20year`, values_from = c(avg_rain, avg_temp))

df1 <- read.csv(getDataPath("09.05.2022_data.csv")) %>% 
  dplyr::select(site, avg_rain_previous_years, avg_rain_previous_months, avg_temp_previous_years, avg_temp_previous_months) %>% 
  distinct()
df1$site <- as.factor(df1$site)

data_og <- read.csv(getDataPath("07.05.2022_data.csv"))
data_og$site <- as.factor(data_og$site)


new_data <- right_join(df1, data_og, by = "site") %>% 
  write.csv(., getDataPath("10.05.2022_fulldata.csv"))
