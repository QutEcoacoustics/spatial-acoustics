library(tidyverse)
library(ggplot2)

rm(list = ls())

set.seed(123)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}


df <- read.csv(getDataPath("19.04.2022_df_final.csv"))

summary(df)


plot_df <- filter(df, general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony" | general_category == "biophony/geophony") %>% 
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

plot_df$RFclass <- as.factor(plot_df$RFclass)
plot_df$period <- as.factor(plot_df$period)
plot_df$date_r <- as.Date.character(plot_df$date_r) %>% 
  sort()
plot_df$new_month <- as.factor(plot_df$new_month)

plot_df$Recording_time <- factor(plot_df$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))
plot_df$week_day <- factor(plot_df$week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

plot_df <- mutate(plot_df, month_char = as.factor(case_when(new_month == "08" ~ "august",
                                          new_month == "09" ~ "september",
                                          new_month == "10" ~ "october")))

#write.csv(plot_df, getDataPath("18.04.2022_df_filtered.csv"))

data <- filter(plot_df, RFclass != "NA" & new_month != "NA") %>% 
  mutate(n_days = 92) %>% 
  select(RFclass, general_category, date_r, week_day, Recording_time, n_days, new_month, site, month_char) %>% 
  na.exclude() %>% 
  group_by(new_month, site) %>% 
  mutate(n_motif =  n()) %>%
  mutate(avg_motif = paste("Motif/day=", format(round(n_motif/n_days, 0)))) %>% 
  mutate(n_motif_char = paste("n motifs=", n_motif, sep = "")) %>%
  mutate(n_days_char = paste("Recorded days=", n_days, sep = "")) %>%
  mutate(month_nmotif_char = paste("N motif", month_char, "=", n_motif, sep = " ")) %>% 
  droplevels()


labels <- select(data, new_month, n_motif, n_days, site) %>% 
  distinct() %>% 
  mutate(new_month = case_when(new_month == "08" ~ "Aug",
                               new_month == "09" ~ "Sep",
                               new_month == "10" ~ "Oct")) %>% 
  mutate(average_motif = paste("Motif/day = ", format(round(n_motif/n_days, 2)), sep = "")) %>% 
  mutate(labels = paste(new_month, average_motif, sep = "\n"))

data$site <- factor(data$site, levels = c("Eungella", "SERF", "Bowra", "BonBon", "Booroopki"))

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

# Hourly plots
data %>% filter(new_month != "NA") %>% 
ggplot(data = ., aes(x = as.factor(Recording_time), fill = RFclass)) + 
  geom_bar() +
  scale_fill_manual(values = c("bird" = "#c51b7d", "birdfroginsect" = "#9ebcda", "birdinsect" = "#e9a3c9", "froginsect" = "#4d9221", "insect" = "#5ab4ac", "bat" = "#3690c0", "batbirdinsect" = "#c994c7", "batfroginsect" = "#3f007d", "birdfrog" = "#ef6548", "birdmammal" = "#a6bddb", "frog" = "#f7fcb9", "frogbird" = "#7bccc4", "froginsectmammal" = "#ccebc5")) +
  labs(fill = "Sound class", x = "Time", y = "% sound class") +
  scale_x_discrete(labels = c("0:00:00" = "0:00", "2:00:00" = "2:00", "4:00:00" = "4:00", "6:00:00" = "6:00", "8:00:00" = "8:00", "10:00:00" = "10:00", "12:00:00" = "12:00", "14:00:00" = "14:00", "16:00:00" = "16:00", "18:00:00" = "18:00", "20:00:00" = "20:00", "22:00:00" = "22:00")) +
  # coord_polar() +
  facet_wrap(~site + month_nmotif_char, scales = "free", nrow = 5, ncol = 3)
  ggsave(getDataPath("Figures", "19.04.2022_BarPlot_hourly_month_site.jpg"), width = 15, height = 9)


  

