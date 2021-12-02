rm(list = ls())

library(ggplot2)
library(tidyverse)


getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

chapter <- "AIndices"

list_files <- list.files(getDataPath(chapter, "9_MergedDF"), pattern = ".csv", full.names = T)

#list_files <- list_files1[1]

for (file in list_files) {
  
  name <- basename(file) %>% 
    str_split(., pattern = "_")
  
  name <- paste(name[[1]][[1]], name[[1]][[2]], sep = "_")
  
  table <- read.csv(file) %>% 
    filter(class != "NA") %>%
    separate(., id, into = c("site", "point", "month", "index", "number", "what"), sep = "_", remove = F) %>% 
    select(., class, id, site, point, month, index, position, index_value, date, time, time_real, reference, fid_what, date_time)
  
  count <- group_by(table, class, index) %>%
    count(.)
  
  
  #write.csv(count, getDataPath(chapter, "SummaryResults", paste("17.11.2021_countdata_", basename(file), sep = "")))
  
  
  sum <- sum(count$n)
  
  nmax <- max(count$n)-10
  
  plot <- ggplot() +
    geom_col(data = count, aes(x = index, y = n, fill = class), position = "dodge") +
    theme_classic() +
    #scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
    annotate(geom = "text", x = 1, y = nmax, label = paste("Total labels: ", sum, sep = "")) +
    ggsave(getDataPath(chapter, "SummaryResults", "Figures", paste("17.11.2021_", name, "_countplot", ".png", sep = "")))  
  
  
}


# table <- read.csv(getDataPath(chapter, "8.11.2021_birdsandinsectsabd.csv")) %>% 
#   filter(Common.name != "Bat - Austronomus") %>% 
#   filter(ï..environ != "all") %>% 
#   droplevels(.)
# 
# count <- table %>% group_by(ï..environ) %>% 
#   count(Feeding.habits)

# ggplot() +
#   geom_col(data = table, aes(x = ï..environ, y = percentage, fill = Feeding.habits), position = "dodge") +
#   #annotate("text", label = count$n) +
#   theme_classic() +
#   scale_fill_manual(values = c("#6e016b", "#88419d", "#8c6bb1", "#3690c0", "#8c96c6", "#9ebcda", "#bfd3e6", "#e0ecf4"), labels = c("Birds - Carnivore", "Birds - Frugivore", "Birds - Granivore", "Insects", "Birds - Insectivore", "Birds - Insectivore/Carnivore", "Birds - Insectivore/Granivore", "Birds - Omnivore")) +
#   labs(x = "Vegetation", fill = "Legend", y = "Proportion") +
#   theme(axis.text.y = element_blank())
# 
# land_var <- read.csv(getDataPath("Fieldwork_Bowra", "26.02.2021_dataSAVINDVI.csv"))
# 
# land_var$aug_ndvi_avg <- as.numeric(land_var$aug_ndvi_avg)
# 
# 
# land_var <-  filter(land_var, NewVegDescription != "") %>% 
#   mutate_at(., vars(NT_N_DIST, NT_W_DIST, NT_S_DIST, NT_E_DIST, NS_N_DIST, NS_W_DIST, NS_S_DIST, NS_E_DIST), ~ replace(., is.na(.), 100)) %>%
#   mutate_at(., vars(NT_N_HEIGHT, NT_S_HEIGHT, NT_W_HEIGHT, NT_E_HEIGHT, NS_N_HEIGHT, NS_S_HEIGHT, NS_E_HEIGHT, NS_W_HEIGHT), ~replace(., is.na(.), 0)) %>%
#   mutate_at(., vars(GC_NF_W, Slope, Aspect, Elevation, DistWater, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, aug_ndvi_avg, aug_savi_avg), ~replace(., is.na(.), 0)) %>%
#   mutate(NT_DIST_AVG = (NT_N_DIST + NT_S_DIST + NT_E_DIST + NT_W_DIST)/4) %>%
#   mutate(NT_HEIGHT_AVG = (NT_N_HEIGHT + NT_S_HEIGHT + NT_E_HEIGHT + NT_W_HEIGHT)/4) %>%
#   mutate(NS_DIST_AVG = (NS_N_DIST + NS_S_DIST + NS_E_DIST + NS_W_DIST)/4) %>%
#   mutate(NS_HEIGHT_AVG = (NS_N_HEIGHT + NS_S_HEIGHT + NS_E_HEIGHT + NS_W_HEIGHT)/4) %>%
#   mutate(GC_NG_AVG = (GC_NG_N + GC_NG_S + GC_NG_E + GC_NG_W)/4) %>%
#   mutate(GC_NF_AVG = (GC_NF_N + GC_NF_S + GC_NF_E + GC_NF_W)/4) %>%
#   mutate(GC_BS_AVG = (GC_BS_N + GC_BS_S + GC_BS_E + GC_BS_W)/4) %>%
#   mutate(GC_LT_AVG = (GC_LT_N + GC_LT_S + GC_LT_E + GC_LT_W)/4) %>%
#   mutate(GC_SH_AVG = (GC_SH_N + GC_SH_S + GC_SH_E + GC_SH_W)/4) %>% 
#   select(., NT_DIST_AVG, NT_HEIGHT_AVG, NS_DIST_AVG, NS_HEIGHT_AVG, GC_NG_AVG, GC_NF_AVG, GC_BS_AVG, GC_SH_AVG, aug_ndvi_avg, CanopyCover, ShrubCover, CanopyHeight, SubcanopyHeight, Slope, Aspect, Elevation, DistWater, Point, NewVegDescription, VegDescription2) %>%
#   droplevels(.)
# 
# 
# ggplot(land_var, aes(x = VegDescription2, y = SubcanopyHeight)) +
#   geom_boxplot() +
#   theme_classic() +
#   labs(x = "Vegetation Description", y = "Subcanopy Height") +
#   scale_x_discrete(limits = c("shrubland (2)", "open shrubland (3)", "sparse shrubland (4)", "woodlands (1)", "very sparse shrubland (5)"), labels =c("shrubland (2)" = "Shrubland", "open shrubland (3)" = "Open shrubland", "sparse shrubland (4)" = "Sparse shrubland", "woodlands (1)" = "Woodland", "very sparse shrubland (5)" = "Very sparse shrubland")) +
#   theme(axis.text = element_text(size = 12)) +
#   ggsave(getDataPath(chapter, "Figures", "22.10.2021_Boxplot_Subcanopy.jpg"))
# 
# 
# ggplot(land_var, aes(x = VegDescription2, y = NS_DIST_AVG)) +
#   geom_boxplot() +
#   theme_classic() +
#   labs(x = "Vegetation Description", y = "Distance to Nearest Shrub") +
#   scale_x_discrete(limits = c("shrubland (2)", "open shrubland (3)", "sparse shrubland (4)", "woodlands (1)", "very sparse shrubland (5)"), labels =c("shrubland (2)" = "Shrubland", "open shrubland (3)" = "Open shrubland", "sparse shrubland (4)" = "Sparse shrubland", "woodlands (1)" = "Woodland", "very sparse shrubland (5)" = "Very sparse shrubland")) +
#   theme(axis.text = element_text(size = 12)) +
#   ggsave(getDataPath(chapter, "Figures", "22.10.2021_Boxplot_nearestshrub.jpg"))
# 
# plot(merged$veg_description2, merged$SubcanopyHeight)
