rm(list = ls())

library(ggplot2)
library(tidyverse)


getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "Chapter1_FineScaleAcousticSurvey"

table <- read.csv(getDataPath(chapter, "7.07.2021_birdsenviron.csv")) %>% 
  filter(Common.name != "Bat - Austronomus" & Common.name != "Insects") %>% 
  droplevels(.)

count <- table %>% group_by(environ) %>% 
  count(Feeding.habits)

ggplot(data = table, y = Feeding.habits) +
  geom_bar(aes(x = environ, fill = Feeding.habits), stat = "count") +
  annotate("text", label = count$n) +
  theme_classic() +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a"), labels = c("generalist", "insectivorous", "other")) +
  labs(x = "vegetation", fill = "Feeding habits")

land_var <- read.csv(getDataPath("Fieldwork_Bowra", "26.02.2021_dataSAVINDVI.csv"))

land_var$aug_ndvi_avg <- as.numeric(land_var$aug_ndvi_avg)


land_var <-  filter(land_var, NewVegDescription != "") %>% 
  mutate_at(., vars(NT_N_DIST, NT_W_DIST, NT_S_DIST, NT_E_DIST, NS_N_DIST, NS_W_DIST, NS_S_DIST, NS_E_DIST), ~ replace(., is.na(.), 100)) %>%
  mutate_at(., vars(NT_N_HEIGHT, NT_S_HEIGHT, NT_W_HEIGHT, NT_E_HEIGHT, NS_N_HEIGHT, NS_S_HEIGHT, NS_E_HEIGHT, NS_W_HEIGHT), ~replace(., is.na(.), 0)) %>%
  mutate_at(., vars(GC_NF_W, Slope, Aspect, Elevation, DistWater, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, aug_ndvi_avg, aug_savi_avg), ~replace(., is.na(.), 0)) %>%
  mutate(NT_DIST_AVG = (NT_N_DIST + NT_S_DIST + NT_E_DIST + NT_W_DIST)/4) %>%
  mutate(NT_HEIGHT_AVG = (NT_N_HEIGHT + NT_S_HEIGHT + NT_E_HEIGHT + NT_W_HEIGHT)/4) %>%
  mutate(NS_DIST_AVG = (NS_N_DIST + NS_S_DIST + NS_E_DIST + NS_W_DIST)/4) %>%
  mutate(NS_HEIGHT_AVG = (NS_N_HEIGHT + NS_S_HEIGHT + NS_E_HEIGHT + NS_W_HEIGHT)/4) %>%
  mutate(GC_NG_AVG = (GC_NG_N + GC_NG_S + GC_NG_E + GC_NG_W)/4) %>%
  mutate(GC_NF_AVG = (GC_NF_N + GC_NF_S + GC_NF_E + GC_NF_W)/4) %>%
  mutate(GC_BS_AVG = (GC_BS_N + GC_BS_S + GC_BS_E + GC_BS_W)/4) %>%
  mutate(GC_LT_AVG = (GC_LT_N + GC_LT_S + GC_LT_E + GC_LT_W)/4) %>%
  mutate(GC_SH_AVG = (GC_SH_N + GC_SH_S + GC_SH_E + GC_SH_W)/4) %>% 
  select(., NT_DIST_AVG, NT_HEIGHT_AVG, NS_DIST_AVG, NS_HEIGHT_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG, aug_ndvi_avg, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, Point, NewVegDescription, VegDescription2) %>%
  droplevels(.)


ggplot(land_var, aes(x = VegDescription2, y = SubcanopyHeight)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Vegetation Description", y = "Subcanopy Height") +
  scale_x_discrete(limits = c("shrubland (2)", "open shrubland (3)", "sparse shrubland (4)", "woodlands (1)", "very sparse shrubland (5)")) +
  ggsave(getDataPath(chapter, "Figures", "Boxplot_Subcanopy.jpg"))


ggplot(land_var, aes(x = VegDescription2, y = NS_DIST_AVG)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Vegetation Description", y = "Distance to Nearest Shrub") +
  scale_x_discrete(limits = c("shrubland (2)", "open shrubland (3)", "sparse shrubland (4)", "woodlands (1)", "very sparse shrubland (5)")) +
  ggsave(getDataPath(chapter, "Figures", "Boxplot_nearestshrub.jpg"))

plot(merged$veg_description2, merged$SubcanopyHeight)
