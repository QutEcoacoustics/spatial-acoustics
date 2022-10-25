library(tidyverse)
library(ggplot2)
library(lubridate)
library(vegan)

rm(list = ls())

set.seed(123)

# set.group <- "bird"

#Splitting the data using a function from dplyr package



getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_og <- read.csv(getDataPath("13.05.2022_fixingdata5.csv")) %>% 
  # mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  # filter(RFclass == "bird" ) %>% 
  # group_by(ID.x, RFclass, date_r) %>% 
  # mutate(n = n()) %>% 
  # mutate(moon_illu = case_when(period =="day" ~ 0,
  #                              TRUE ~ moon_illu)) %>% 
  # rowwise() %>% 
  # mutate(., mean_temp = mean(c(temp_max,temp_min))) %>%
  # mutate(., mean_temp = mean(c(temp_max,temp_min))) %>%
  mutate(location = case_when(ID.x == "BonBon_WetA" ~ "7",
                              ID.x == "BonBon_DryA" ~ "6",
                              ID.x == "Booroopki_DryA" ~ "305",
                              ID.x == "Booroopki_WetA" ~ "306",
                              ID.x == "Bowra_DryA" ~ "259",
                              ID.x == "Bowra_WetA" ~ "258",
                              ID.x == "Eungella_DryA" ~ "110",
                              ID.x == "Eungella_WetA" ~ "111",
                              ID.x == "SERF_DryA" ~ "253",
                              ID.x == "SERF_WetA" ~ "254")) %>% 
  dplyr::select(everything(), -c(Recording_time, day, week, id, id_path, fid_what, ca_class_6_325)) %>% 
  ungroup() %>% 
  # filter(n > 3) %>% 
  distinct()

#General multinomial - between sites ----

dataframe_landscape <- data_og %>%  
  dplyr::select(., ID.x, RFclass, site, bvg_char, temp_max, natural_cover_325, contag_landscape_325, tca_landscape_325) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, site, bvg_char, temp_max, natural_cover_325, contag_landscape_325, tca_landscape_325) %>%
  mutate(n = n(),
         natural_cover_325 = round(natural_cover_325,2),
         tca_landscape_325 = round(tca_landscape_325,2),
         # water_3k = round(water_3k,2),
         contag_landscape_325 = round(contag_landscape_325,2),
         temp_total = round(mean(temp_max), 2)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, site, ID.x, bvg_char, everything(), -c(temp_max)) %>% 
  filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n")
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0



dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(4:7), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

nmds <- metaMDS(dataframe_norm_landscape[,c(8:10)], k = 2, trymax = 100, distance = "jaccard")

en <- envfit(nmds, dataframe_norm_landscape[,3:7], permutations = 999, na.rm = T,  distance = "jaccard")
en

plot(nmds$species)
plot(en)

data.scores = as.data.frame(scores(nmds)$sites)

#add 'season' column as before
data.scores$site = dataframe_land_wide$site
data.scores$bvg = dataframe_land_wide$bvg_char
data.scores$id <- rownames(data.scores)


species.scores <- as.data.frame(scores(nmds, "species"))
species.scores$var <- rownames(species.scores)

en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
en_coord_cont$variables <- rownames(en_coord_cont)
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)



ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = site), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("#fdbb84", "#8c6bb1", "#8c510a", "#1b7837", "#4393c3"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, shape = var), size = 3) +
  geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
            label = c("dry rainforest", "EOF - shruby understory", "EW - grassy understory", "EW - shruby understory", "Mulga", "Saltbush shrub", "Tropical rainforest"), colour = "navy", fontface = "bold") + 
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = c("natural cover (3k)", "number of patches (3k)", "total core area (325m)", "mean temperature")) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Site", shape = "Group")
ggsave(getDataPath("Figures", "landscape_nmds.jpg"))

# bvg_char, temp_max, natural_cover_3k, np_landscape_3k, tca_landscape_325

# temp_total, moon, natural_cover_3k, np_landscape_3k, tca_landscape_325



PERMANOVA <- adonis2(dataframe_norm_landscape[,9:11]~ dataframe_norm_landscape$natural_cover_3k + dataframe_norm_landscape$np_landscape_3k +  dataframe_norm_landscape$tca_landscape_325 + dataframe_norm_landscape$bvg_char + dataframe_norm_landscape$moon + dataframe_norm_landscape$temp_total)

PERMANOVA

#Eungella ----

filtered <- filter(data_og, site == "Eungella" & RFclass != "bat")

dataframe_landscape <- filtered %>%  
  dplyr::select(., ID.x, RFclass, temp_max, period, bvg_char) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, temp_max, period) %>%
  mutate(n = n(), 
         # natural_cover_3k = round(natural_cover_3k,2),
         # tca_landscape_325 = round(tca_landscape_325,2),
         # # water_3k = round(water_3k,2),
         # np_landscape_3k = round(np_landscape_3k,2),
         temp_total = round(mean(temp_max), 2)) %>% 
         # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything(), -c(temp_max)) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n")
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(4), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

# nmds <- metaMDS(dataframe_norm_landscape[,c(6:8)], k = 2, trymax = 100)
# 
# en <- envfit(nmds, dataframe_norm_landscape[,3:4], permutations = 999, na.rm = T)
# en
# 
# plot(nmds$species)
# plot(en)
# 
# data.scores = as.data.frame(scores(nmds)$sites)
# 
# #add 'season' column as before
# data.scores$site = dataframe_land_wide$ID.x
# data.scores$bvg = dataframe_land_wide$bvg_char
# data.scores$id <- rownames(data.scores)
# 
# 
# species.scores <- as.data.frame(scores(nmds, "species"))
# species.scores$var <- rownames(species.scores)
# 
# en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
# en_coord_cont$variables <- rownames(en_coord_cont)
# en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)
# 
# 
# 
# ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = bvg), size = 3, alpha = 0.5) + 
#   # scale_colour_manual(values = c("orange", "steelblue"))  + 
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
#   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
#   geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, shape = var), size = 3) +
#   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
#   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont)) + 
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) + 
#   labs(colour = "Site", shape = "Group")
# ggsave(getDataPath("Figures", "landscape_nmds_colouredbvg.jpg"))

# temp_total, moon, natural_cover_3k, np_landscape_3k, tca_landscape_325

PERMANOVA <- adonis2(dataframe_norm_landscape[,5:7]~ dataframe_norm_landscape$period + dataframe_norm_landscape$temp_total)
PERMANOVA

result$conv <- as.character(nmds$converged)
result$stress <- as.numeric(nmds$stress)
result$permanova_F <- as.numeric(PERMANOVA$F.Model[1])
result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
result <- as.data.frame(result)

#SERF ----

filtered <- filter(data_og, site == "SERF" & RFclass != "bat" & RFclass != "mammal")

dataframe_landscape <- filtered %>%  
  dplyr::select(., ID.x, RFclass, temp_max, period, bvg_char) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, temp_max, period) %>%
  mutate(n = n(), 
         # natural_cover_3k = round(natural_cover_3k,2),
         # tca_landscape_325 = round(tca_landscape_325,2),
         # # water_3k = round(water_3k,2),
         # np_landscape_3k = round(np_landscape_3k,2),
         temp_total = round(mean(temp_max), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything(), -c(temp_max)) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n")
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(4), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

# nmds <- metaMDS(dataframe_norm_landscape[,c(6:8)], k = 2, trymax = 100)
# 
# en <- envfit(nmds, dataframe_norm_landscape[,3:4], permutations = 999, na.rm = T)
# en
# 
# plot(nmds$species)
# plot(en)
# 
# data.scores = as.data.frame(scores(nmds)$sites)
# 
# #add 'season' column as before
# data.scores$site = dataframe_land_wide$ID.x
# data.scores$bvg = dataframe_land_wide$bvg_char
# data.scores$id <- rownames(data.scores)
# 
# 
# species.scores <- as.data.frame(scores(nmds, "species"))
# species.scores$var <- rownames(species.scores)
# 
# en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
# en_coord_cont$variables <- rownames(en_coord_cont)
# en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)
# 
# 
# 
# ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = bvg), size = 3, alpha = 0.5) + 
#   # scale_colour_manual(values = c("orange", "steelblue"))  + 
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
#   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
#   geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, shape = var), size = 3) +
#   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
#   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont)) + 
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) + 
#   labs(colour = "Site", shape = "Group")
# ggsave(getDataPath("Figures", "landscape_nmds_colouredbvg.jpg"))

# temp_total, moon, natural_cover_3k, np_landscape_3k, tca_landscape_325

PERMANOVA <- adonis2(dataframe_norm_landscape[,5:7]~ dataframe_norm_landscape$period + dataframe_norm_landscape$temp_total)
PERMANOVA

# result$conv <- as.character(nmds$converged)
# result$stress <- as.numeric(nmds$stress)
# result$permanova_F <- as.numeric(PERMANOVA$F.Model[1])
# result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
# result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
# result <- as.data.frame(result)


#Bowra ----

filtered <- filter(data_og, site == "Bowra" & RFclass != "bat" & RFclass != "mammal")

dataframe_landscape <- filtered %>%  
  dplyr::select(., ID.x, RFclass, bvg_char, period, temp_max, ndvi_mean, natural_cover_325, rain_value) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, period, temp_max, ndvi_mean, natural_cover_325, rain_value) %>%
  mutate(n = n(), 
         natural_cover_325 = round(natural_cover_325,2),
         rain_value = round(mean(rain_value),2),
         ndvi_mean = round(mean(ndvi_mean),2),
         # np_landscape_3k = round(np_landscape_3k,2),
         temp_total = round(mean(temp_max), 2)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything(), -c(temp_max)) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n")
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(4:7), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

# nmds <- metaMDS(dataframe_norm_landscape[,c(6:8)], k = 2, trymax = 100)
# 
# en <- envfit(nmds, dataframe_norm_landscape[,3:4], permutations = 999, na.rm = T)
# en
# 
# plot(nmds$species)
# plot(en)
# 
# data.scores = as.data.frame(scores(nmds)$sites)
# 
# #add 'season' column as before
# data.scores$site = dataframe_land_wide$ID.x
# data.scores$bvg = dataframe_land_wide$bvg_char
# data.scores$id <- rownames(data.scores)
# 
# 
# species.scores <- as.data.frame(scores(nmds, "species"))
# species.scores$var <- rownames(species.scores)
# 
# en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
# en_coord_cont$variables <- rownames(en_coord_cont)
# en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)
# 
# 
# 
# ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = bvg), size = 3, alpha = 0.5) + 
#   # scale_colour_manual(values = c("orange", "steelblue"))  + 
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
#   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
#   geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, shape = var), size = 3) +
#   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
#   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont)) + 
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) + 
#   labs(colour = "Site", shape = "Group")
# ggsave(getDataPath("Figures", "landscape_nmds_colouredbvg.jpg"))

# temp_total, moon, natural_cover_3k, np_landscape_3k, tca_landscape_325

PERMANOVA <- adonis2(dataframe_norm_landscape[,8:10]~ dataframe_norm_landscape$period + dataframe_norm_landscape$temp_total + dataframe_norm_landscape$ndvi_mean + dataframe_norm_landscape$natural_cover_325 + dataframe_norm_landscape$rain_value)
PERMANOVA

PERMANOVA2 <- adonis2(dataframe_norm_landscape[,8:10]~ dataframe_norm_landscape$period + dataframe_norm_landscape$temp_total + dataframe_norm_landscape$natural_cover_325)
PERMANOVA2


# result$conv <- as.character(nmds$converged)
# result$stress <- as.numeric(nmds$stress)
# result$permanova_F <- as.numeric(PERMANOVA$F.Model[1])
# result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
# result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
# result <- as.data.frame(result)


#BonBon ----

filtered <- filter(data_og, site == "BonBon" & RFclass != "bat" & RFclass != "mammal")

dataframe_landscape <- filtered %>%  
  dplyr::select(., ID.x, RFclass, bvg_char, contag_landscape_325, ndvi_mean, moon_illu, rain_value, np_landscape_325) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, contag_landscape_325, ndvi_mean, moon_illu, rain_value, np_landscape_325) %>%
  mutate(n = n(), 
         contag_landscape_325 = round(contag_landscape_325,2),
         rain_value = round(mean(rain_value),3),
         ndvi_mean = round(mean(ndvi_mean),3),
         np_landscape_325 = round(np_landscape_325,2),
         moon_illu = round(mean(moon_illu), 3)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(3:7), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

# nmds <- metaMDS(dataframe_norm_landscape[,c(6:8)], k = 2, trymax = 100)
# 
# en <- envfit(nmds, dataframe_norm_landscape[,3:4], permutations = 999, na.rm = T)
# en
# 
# plot(nmds$species)
# plot(en)
# 
# data.scores = as.data.frame(scores(nmds)$sites)
# 
# #add 'season' column as before
# data.scores$site = dataframe_land_wide$ID.x
# data.scores$bvg = dataframe_land_wide$bvg_char
# data.scores$id <- rownames(data.scores)
# 
# 
# species.scores <- as.data.frame(scores(nmds, "species"))
# species.scores$var <- rownames(species.scores)
# 
# en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
# en_coord_cont$variables <- rownames(en_coord_cont)
# en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)
# 
# 
# 
# ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = bvg), size = 3, alpha = 0.5) + 
#   # scale_colour_manual(values = c("orange", "steelblue"))  + 
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
#   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
#   geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, shape = var), size = 3) +
#   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
#   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont)) + 
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) + 
#   labs(colour = "Site", shape = "Group")
# ggsave(getDataPath("Figures", "landscape_nmds_colouredbvg.jpg"))

# temp_total, moon, natural_cover_3k, np_landscape_3k, tca_landscape_325

PERMANOVA <- adonis2(dataframe_norm_landscape[,8:10]~ dataframe_norm_landscape$contag_landscape_325 + dataframe_norm_landscape$ndvi_mean + dataframe_norm_landscape$moon_illu + dataframe_norm_landscape$rain_value + dataframe_norm_landscape$np_landscape_325)
PERMANOVA

PERMANOVA2 <- adonis2(dataframe_norm_landscape[,8:10]~ dataframe_norm_landscape$np_landscape_325)
PERMANOVA2


# result$conv <- as.character(nmds$converged)
# result$stress <- as.numeric(nmds$stress)
# result$permanova_F <- as.numeric(PERMANOVA$F.Model[1])
# result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
# result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
# result <- as.data.frame(result)


#Booroopki ----

filtered <- filter(data_og, site == "Booroopki" & RFclass != "bat" & RFclass != "mammal")

dataframe_landscape <- filtered %>%  
  dplyr::select(., ID.x, RFclass, bvg_char, period, rain_value, np_landscape_325) %>% 
  # group_by(ID.x, RFclass) %>% 
  # # mutate(temp_total = round(mean(temp_max)),
  # #        moon = round(mean(moon_illu), 2),  
  #   mutate(natural_cover_3k = round(natural_cover_3k,2),
  #        tca_landscape_325 = round(tca_landscape_325,2),
  #        # water_3k = round(water_3k,2),
  #        np_landscape_3k = round(np_landscape_3k,2)) %>% 
  # ungroup() %>% 
  group_by(RFclass, ID.x, period, rain_value, np_landscape_325) %>%
  mutate(n = n(), 
         # contag_landscape_325 = round(contag_landscape_325,2),
         rain_value = round(mean(rain_value),3),
         # ndvi_mean = round(mean(ndvi_mean),3),
         np_landscape_325 = round(np_landscape_325,2)) %>% 
         # moon_illu = round(mean(moon_illu), 3)) %>% 
  # moon = round(moon_illu,4)) %>% 
  ungroup() %>% 
  # group_by(ID.x) %>% 
  # ungroup() %>% 
  dplyr::select(., RFclass, ID.x, bvg_char, everything()) %>% 
  # filter(n > 3) %>% 
  distinct() %>% 
  droplevels()

# dataframe$id_number <- as.factor(dataframe$id_number)
# 
# rownames(dataframe) <- dataframe$id_number

# dataframe <- dplyr::select(dataframe, everything()) %>% 
#   distinct()

# dataframe$np_landscape_3k <- as.numeric(dataframe$np_landscape_3k)
# dataframe$contag_landscape_325 <- as.numeric(dataframe$contag_landscape_325)


dataframe_land_wide <- pivot_wider(dataframe_landscape, names_from = "RFclass", values_from = "n", values_fn = sum)
dataframe_land_wide$bird[is.na(dataframe_land_wide$bird)] <- 0
dataframe_land_wide$frog[is.na(dataframe_land_wide$frog)] <- 0
dataframe_land_wide$insect[is.na(dataframe_land_wide$insect)] <- 0


dataframe_norm_landscape <- dataframe_land_wide %>% mutate_at(c(6:8), ~decostand(., method = "range") %>% as.vector(.)) %>% 
  droplevels()

# nmds <- metaMDS(dataframe_norm_landscape[,c(6:8)], k = 2, trymax = 100)
# 
# en <- envfit(nmds, dataframe_norm_landscape[,3:4], permutations = 999, na.rm = T)
# en
# 
# plot(nmds$species)
# plot(en)
# 
# data.scores = as.data.frame(scores(nmds)$sites)
# 
# #add 'season' column as before
# data.scores$site = dataframe_land_wide$ID.x
# data.scores$bvg = dataframe_land_wide$bvg_char
# data.scores$id <- rownames(data.scores)
# 
# 
# species.scores <- as.data.frame(scores(nmds, "species"))
# species.scores$var <- rownames(species.scores)
# 
# en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
# en_coord_cont$variables <- rownames(en_coord_cont)
# en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)
# 
# 
# 
# ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = bvg), size = 3, alpha = 0.5) + 
#   # scale_colour_manual(values = c("orange", "steelblue"))  + 
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
#   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
#   geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, shape = var), size = 3) +
#   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
#   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont)) + 
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) + 
#   labs(colour = "Site", shape = "Group")
# ggsave(getDataPath("Figures", "landscape_nmds_colouredbvg.jpg"))

# temp_total, moon, natural_cover_3k, np_landscape_3k, tca_landscape_325

PERMANOVA <- adonis2(dataframe_norm_landscape[,6:8]~ dataframe_norm_landscape$period + dataframe_norm_landscape$rain_value + dataframe_norm_landscape$np_landscape_325)
PERMANOVA

# result$conv <- as.character(nmds$converged)
# result$stress <- as.numeric(nmds$stress)
# result$permanova_F <- as.numeric(PERMANOVA$F.Model[1])
# result$permanova_R2 <- as.numeric(PERMANOVA$aov.tab$R2[1])
# result$permanova_p <- as.numeric(PERMANOVA$aov.tab$`Pr(>F)`[1])
# result <- as.data.frame(result)



