rm(list = ls())

library(tidyverse)
library(ggplot2)
library(lubridate)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}
AICc.PERMANOVA2 <- function(adonis2.model) {
  
  # check to see if object is an adonis2 model...
  
  if (is.na(adonis2.model$SumOfSqs[1]))
    stop("object not output of adonis2 {vegan} ")
  
  # Ok, now extract appropriate terms from the adonis model Calculating AICc
  # using residual sum of squares (RSS or SSE) since I don't think that adonis
  # returns something I can use as a likelihood function... maximum likelihood
  # and MSE estimates are the same when distribution is gaussian See e.g.
  # https://www.jessicayung.com/mse-as-maximum-likelihood/;
  # https://towardsdatascience.com/probability-concepts-explained-maximum-likelihood-estimation-c7b4342fdbb1
  # So using RSS or MSE estimates is fine as long as the residuals are
  # Gaussian https://robjhyndman.com/hyndsight/aic/ If models have different
  # conditional likelihoods then AIC is not valid. However, comparing models
  # with different error distributions is ok (above link).
  
  
  RSS <- adonis2.model$SumOfSqs[ length(adonis2.model$SumOfSqs) - 1 ]
  MSE <- RSS / adonis2.model$Df[ length(adonis2.model$Df) - 1 ]
  
  nn <- adonis2.model$Df[ length(adonis2.model$Df) ] + 1
  
  k <- nn - adonis2.model$Df[ length(adonis2.model$Df) - 1 ]
  
  
  # AIC : 2*k + n*ln(RSS/n)
  # AICc: AIC + [2k(k+1)]/(n-k-1)
  
  # based on https://en.wikipedia.org/wiki/Akaike_information_criterion;
  # https://www.statisticshowto.datasciencecentral.com/akaikes-information-criterion/ ;
  # https://www.researchgate.net/post/What_is_the_AIC_formula;
  # http://avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf;
  # https://medium.com/better-programming/data-science-modeling-how-to-use-linear-regression-with-python-fdf6ca5481be 
  
  # AIC.g is generalized version of AIC = 2k + n [Ln( 2(pi) RSS/n ) + 1]
  # AIC.pi = k + n [Ln( 2(pi) RSS/(n-k) ) +1],
  
  AIC <- 2*k + nn*log(RSS/nn)
  AIC.g <- 2*k + nn * (1 + log( 2 * pi * RSS / nn))
  AIC.MSE <- 2*k + nn * log(MSE)
  AIC.pi <- k + nn*(1 + log( 2*pi*RSS/(nn-k) )   )
  AICc <- AIC + (2*k*(k + 1))/(nn - k - 1)
  AICc.MSE <- AIC.MSE + (2*k*(k + 1))/(nn - k - 1)
  AICc.pi <- AIC.pi + (2*k*(k + 1))/(nn - k - 1)
  
  output <- list("AIC" = AIC, "AICc" = AICc, "AIC.g" = AIC.g, 
                 "AIC.MSE" = AIC.MSE, "AICc.MSE" = AICc.MSE,
                 "AIC.pi" = AIC.pi, "AICc.pi" = AICc.pi, "k" = k, "N" = nn)
  
  return(output)   
  
}

data_og <- read.csv(getDataPath("13.05.2022_fixingdata5.csv")) %>% 
  # mutate_at(c(3:6,47,48), ~(scale(.) %>% as.vector(.))) %>% 
  # filter(RFclass == "bird" ) %>% 
  group_by(ID.x, RFclass, date_r) %>% 
  mutate(n = n()) %>% 
  # mutate(moon_illu = case_when(period =="day" ~ 0,
  #                              TRUE ~ moon_illu)) %>% 
  rowwise() %>% 
  mutate(., mean_temp = mean(c(temp_max,temp_min))) %>%
  dplyr::select(n, everything(), -c(Recording_time, day, week, id, id_path, fid_what, -ca_class_6_325)) %>% 
  mutate(., bvg = case_when(ID.x == "SERF_DryA" ~ as.numeric(9),
                            ID.x == "SERF_WetA" ~ as.numeric(2),
                                 TRUE ~ as.numeric(bvg))) %>% 
  mutate(., bvg_char = case_when(bvg == 2 ~ "subtropical_rainforest",
                              bvg == 9 ~ "euc_wood_grassy_under",
                              TRUE ~ bvg_char)) %>% 
  mutate(., veg_type = case_when(bvg == 2 ~ "rainforest_vinethickets",
                                 bvg == 4 ~ "euc_open",
                                 bvg == 8 ~ "euc_wood", 
                                 bvg == 9 ~ "euc_wood",
                                 bvg == 20 ~ "acacia_wood",
                                 bvg == 31 ~ "shrubland",
                                 bvg == 62 ~ "rainforest_vinethickets")) %>% 
  mutate(., understory = case_when(bvg == 2 ~ "rainforest",
                                   bvg == 4 ~ "shrub",
                                   bvg == 8 ~ "shrub", 
                                   bvg == 9 ~ "tussock_grass",
                                   bvg == 20 ~ "tussockgrass_forbs",
                                   bvg == 31 ~ "shrub",
                                   bvg == 62 ~ "rainforest")) %>%
  mutate(ID = case_when(ID.x == "Booroopki_DryA" ~ "BRP_1",
                                     ID.x == "Booroopki_WetA" ~ "BRP_2",
                                     ID.x == "BonBon_DryA" ~ "BNB_1",
                                     ID.x == "BonBon_WetA" ~ "BNB_2",
                                     ID.x == "Bowra_DryA" ~ "BWR_1",
                                     ID.x == "Bowra_WetA" ~ "BWR_2",
                                     ID.x == "Eungella_DryA" ~ "ENG_1",
                                     ID.x == "Eungella_WetA" ~ "ENG_2",
                                     ID.x == "SERF_DryA" ~ "SRF_1",
                                     ID.x == "SERF_WetA" ~ "SRF_2")) %>% 
  mutate(ID_new = case_when(ID.x == "Booroopki_DryA" ~ "Booroopki1",
                        ID.x == "Booroopki_WetA" ~ "Booroopki2",
                        ID.x == "BonBon_DryA" ~ "BonBon1",
                        ID.x == "BonBon_WetA" ~ "BonBon2",
                        ID.x == "Bowra_DryA" ~ "Bowra1",
                        ID.x == "Bowra_WetA" ~ "Bowra2",
                        ID.x == "Eungella_DryA" ~ "Eungella1",
                        ID.x == "Eungella_WetA" ~ "Eungella2",
                        ID.x == "SERF_DryA" ~ "SERF1",
                        ID.x == "SERF_WetA" ~ "SERF2")) %>% 
  mutate(tca_landscape_325 = round(tca_landscape_325)) %>% 
  filter(n > 2) %>% 
  distinct() %>% 
  droplevels()

data_og$bvg_char <- factor(data_og$bvg_char, levels = c("dry_rainforest", "subtropical_rainforest", "mulga_wood_grass_forbs", "euc_wood_grassy_under", "euc_wood_shruby_under", "euc_open_shruby_under", "saltbush_shrub"))

aus <- ozmap()

states <- ozmap() %>% filter(NAME == "Queensland" | NAME == "Victoria" | NAME == "South Australia")


plot_data <- data_og %>% ungroup() %>% 
  select(lat, lon, bvg_char, site, ID_new) %>% 
  mutate(lat = round(lat, 3),
         lon = round(lon, 3),
         bvg_char = case_when(bvg_char == "dry_rainforest" ~ "Dry rainforest",
                              bvg_char == "euc_open_shruby_under" ~ "Euc. open forest/shrubby understorey",
                              bvg_char == "euc_wood_grassy_under" ~ "Euc. woodlands/tussock grass understorey",
                              bvg_char == "euc_wood_shruby_under" ~ "Euc. woodland/shrubby understorey",
                              bvg_char == "mulga_wood_grass_forbs" ~ "Mulga woodlands/tussock grass/forbs", 
                              bvg_char == "saltbush_shrub" ~ "Saltbush/bluebush shrublands",
                              bvg_char == "subtropical_rainforest" ~ "Sub-tropical rainforest"), 
         label = paste(ID_new, bvg_char, sep = ": ")) %>% 
  distinct()

(base_bvg <- ggplot(aus) + 
    geom_sf() +
    geom_point(plot_data, mapping = aes(x = lon, y = lat), size = 3) +
    # facet_wrap(.~NAME) +
    # scale_colour_viridis_b() +
    geom_label_repel(plot_data, mapping = aes( x = lon, y = lat, label = bvg_char), force_pull = 2, min.segment.length = 0.2) +
    theme_bw())

cor.test(data_og$ebi_max, data_og$ndvi_mean)

(base_complete <- ggplot(aus) + 
    geom_sf() +
    geom_point(plot_data, mapping = aes(x = lon, y = lat), size = 3) +
    # facet_wrap(.~NAME) +
    # scale_colour_viridis_b() +
    geom_label_repel(plot_data, mapping = aes( x = lon, y = lat, label = label), force_pull = 2, min.segment.length = 0.2) +
    theme_bw())
ggsave(getDataPath("Figures", "StudySites.jpg"), width = 12, height = 10)

ggplot(data_og, aes(x = ID, y = mean_temp, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "25.05.2022_temp.jpg"))

ggplot(data_og, aes(x = ID, y = natural_cover_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "25.05.2022_naturalcover.jpg"))

ggplot(data_og, aes(x = ID, y = np_landscape_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "25.05.2022_np.jpg"))

ggplot(data_og, aes(x = ID, y = tca_landscape_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "25.05.2022_tca.jpg"))

bvg_labs <- c("Dry rainforest", "Euc. open forest/shrubby understorey", "Euc. woodlands/tussock grass understorey", "Euc. woodland/shrubby understorey", "Mulga woodlands/tussock grass/forbs", "Saltbush/bluebush shrublands", "Sub-tropical rainforest")
names(bvg_labs) <- c("dry_rainforest", "euc_open_shruby_under", "euc_wood_grassy_under", "euc_wood_shruby_under", "mulga_wood_grass_forbs", "saltbush_shrub", "subtropical_rainforest")

data_og <- data_og %>% mutate(NC = paste("NC: ", round(natural_cover_325), sep = ""),
                              TCA = paste("TCA: ", tca_landscape_325, sep = ""),
                              CNT = paste("CNT: ", round(contag_landscape_325), sep = ""),
                              labels = paste(ID_new, NC, TCA, CNT, sep = "\n"))
data_og$labels <- factor(data_og$labels, levels = c("Eungella2\nNC: 31\nTCA: 26\nCNT: 0", "SERF2\nNC: 18\nTCA: 11\nCNT: 24", "Bowra1\nNC: 32\nTCA: 26\nCNT: 97", "Bowra2\nNC: 16\nTCA: 5\nCNT: 21", "SERF1\nNC: 27\nTCA: 19\nCNT: 52", "Booroopki1\nNC: 30\nTCA: 22\nCNT: 76", "Booroopki2\nNC: 17\nTCA: 17\nCNT: 26", "Eungella1\nNC: 31\nTCA: 22\nCNT: 81", "BonBon1\nNC: 1\nTCA: 23\nCNT: 84", "BonBon2\nNC: 0\nTCA: 25\nCNT: 91"))

ggplot(data_og, aes(x = ID_new, y = n, colour = RFclass)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(.~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs))
ggsave(getDataPath("Figures", "30.05.2022_n_persite.jpg"))

data_og %>% filter(RFclass == "bird") %>% 
  ggplot(., aes(x =labels, y = n, fill = bvg_char)) +
  geom_boxplot() +
  theme_bw() +
  # annotate(geom = "text", x = 1:nrow(data_og), y = 1, label = data_og$natural_cover_325) +
  scale_fill_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs)) +
  xlab("Sites + Landscape metrics")
ggsave(getDataPath("Figures", "02.06.2022_birdsperenviro.jpg"), height = 10, width = 20)

data_og %>% filter(RFclass == "insect") %>% 
  ggplot(., aes(x =labels, y = n, fill = bvg_char)) +
  geom_boxplot() +
  theme_bw() +
  # annotate(geom = "text", x = 1:nrow(data_og), y = 1, label = data_og$natural_cover_325) +
  scale_fill_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs)) +
 xlab("Sites + Landscape metrics")
ggsave(getDataPath("Figures", "02.06.2022_insectsperenviro.jpg"), height = 10, width = 20)

data_og %>% filter(RFclass == "frog") %>% 
  ggplot(., aes(x =labels, y = n, fill = bvg_char)) +
  geom_boxplot() +
  theme_bw() +
  # annotate(geom = "text", x = 1:nrow(data_og), y = 1, label = data_og$natural_cover_325) +
  scale_fill_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs)) +
  xlab("Sites + Landscape metrics")
ggsave(getDataPath("Figures", "02.06.2022_frogperenviro.jpg"), height = 10, width = 20)

ggplot(data_og, aes(x = mean_temp, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth(span = 100) +
  theme_bw() +
  # annotate(geom = "text", x = 1:nrow(data_og), y = 1, label = data_og$natural_cover_325) +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"), labels = bvg_labs) +
  labs(colour = "Vegetation groups", x = "Mean temperature") +
  facet_grid(RFclass~ID_new)
ggsave(getDataPath("Figures", "02.06.2022_allgroupspertemp.jpg"), height = 10, width = 20)


  ggplot(data_og, aes(x =labels, y = n, fill = bvg_char)) +
  geom_boxplot() +
  theme_bw() +
  # annotate(geom = "text", x = 1:nrow(data_og), y = 1, label = data_og$natural_cover_325) +
  scale_fill_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(RFclass~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs)) +
  xlab("Sites + Landscape metrics")
ggsave(getDataPath("Figures", "02.06.2022_allgroupsperenviro.jpg"), height = 10, width = 20)

library(cowplot)

plot_grid(base_bvg, nat_cover, rel_widths = c(1.5, 1.5))
ggsave(getDataPath("Figures", "01.06.2022_n_pernatcover.jpg"), width = 19, height = 11)

tca <- ggplot(data_og, aes(x = as.factor(round(tca_landscape_325)), y = n, fill = RFclass)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("bird" = "#c51b7d", "insect" = "#5ab4ac", "frog" = "#f7fcb9")) +
  facet_wrap(.~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs)) +
  xlab("Total Core Area (ha)")

plot_grid(base_bvg, tca, rel_widths = c(1.5, 1.5))
ggsave(getDataPath("Figures", "01.06.2022_n_pertca.jpg"), width = 19, height = 11)


contag <- ggplot(data_og, aes(x = as.factor(round(contag_landscape_325)), y = n, fill = RFclass)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("bird" = "#c51b7d", "insect" = "#5ab4ac", "frog" = "#f7fcb9")) +
  facet_wrap(.~bvg_char, scales = "free_x", labeller = labeller(bvg_char = bvg_labs)) +
  xlab("Connectivity (range 0 to 1)")

plot_grid(base_bvg, contag, rel_widths = c(1.5, 1.5))
ggsave(getDataPath("Figures", "01.06.2022_n_percontag.jpg"), width = 19, height = 11)

library(sf)
library(sp)
library(ozmaps)
library(ggrepel)

data_og <- mutate(data_og, NAME = case_when(site == "Eungella" | site == "SERF" | site == "Bowra" ~ "Queensland",
                                             site == "BonBon" ~ "South Australia",
                                             site == "Booroopki" ~ "Victoria"))



plot_data <- data_og %>% ungroup() %>% 
  select(lat, lon, bvg_char, site) %>% 
  mutate(lat = round(lat, 3),
         lon = round(lon, 3)) %>% 
  distinct()

(base_site <- ggplot(aus) + 
    geom_sf() +
    geom_point(plot_data, mapping = aes(x = lon, y = lat), size = 2) +
    # facet_wrap(.~NAME) +
    # scale_colour_viridis_b() +
    geom_label_repel(plot_data, mapping = aes(x = lon, y = lat, label = site)) +
    theme_bw())


#Birds----



filtered <- filter(data_og, RFclass == "bird")
filtered$date_r <- dmy(filtered$date_r)

#Plots ----

ggplot(filtered, aes(x = ID, y = mean_temp, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_temp_birds.jpg"))

ggplot(filtered, aes(x = ID, y = natural_cover_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_naturalcover_birds.jpg"))

ggplot(filtered, aes(x = ID, y = contag_landscape_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_np_birds.jpg"))

ggplot(filtered, aes(x = ID, y = tca_landscape_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_tca_birds.jpg"))

ggplot(filtered, aes(x = ID, y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "23.05.2022_boxplot_enviro_birds.jpg"))


ggplot(data = filtered, aes(x = date_r, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_bird_bvg_time.jpg"))

filtered %>%
  ggplot(aes(x = mean_temp, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~ site)
ggsave(getDataPath("Figures", "27.05.2022_bird_veg_temp.jpg"))

ggplot(filtered, aes(x = moon_illu, y = n, colour = bvg_char)) +
  geom_point() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  geom_smooth(se = F, method = "loess", na.rm = T, span = 100) +
  facet_grid(.~site)
  ggsave(getDataPath("Figures", "23.05.2022_bird_moon.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(natural_cover_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(name = "Vegetation groups", labels = c("Dry rainforest", "Eucalyptus open forest - shrubby understorey", "Eucalyptus woodlands - tussock grass understorey", "Eucalyptus woodland - shrubby understorey", "Mulga woodlands +/- tussock grass +/- forbs", "Saltbush and bluebush shrublands", "Sub-tropical rainforest"), values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  xlab("Natural cover - m2") +
  stat_n_text() +
  facet_grid(.~ site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_bird_naturalcover.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(contag_landscape_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(name = "Vegetation groups", labels = c("Dry rainforest", "Eucalyptus open forest - shrubby understorey", "Eucalyptus woodlands - tussock grass understorey", "Eucalyptus woodland - shrubby understorey", "Mulga woodlands +/- tussock grass +/- forbs", "Saltbush and bluebush shrublands", "Sub-tropical rainforest"), values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  xlab("Connectivity") +
  stat_n_text() +
  facet_grid(.~ site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_bird_contag.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(tca_landscape_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(name = "Vegetation groups", labels = c("Dry rainforest", "Eucalyptus open forest - shrubby understorey", "Eucalyptus woodlands - tussock grass understorey", "Eucalyptus woodland - shrubby understorey", "Mulga woodlands +/- tussock grass +/- forbs", "Saltbush and bluebush shrublands", "Sub-tropical rainforest"), values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  xlab("Total core area") +
  stat_n_text() +
facet_grid(.~site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_bird_tca.jpg"))



#PERMANOVA ----

df_bird_wide <- pivot_wider(filtered, names_from = "ID", values_from = "n")
df_bird_wide$BRP_1[is.na(df_bird_wide$BRP_1)] <- 0
df_bird_wide$BRP_2[is.na(df_bird_wide$BRP_2)] <- 0
df_bird_wide$BWR_1[is.na(df_bird_wide$BWR_1)] <- 0
df_bird_wide$BWR_2[is.na(df_bird_wide$BWR_2)] <- 0
df_bird_wide$BNB_1[is.na(df_bird_wide$BNB_1)] <- 0
df_bird_wide$BNB_2[is.na(df_bird_wide$BNB_2)] <- 0
df_bird_wide$ENG_1[is.na(df_bird_wide$ENG_1)] <- 0
df_bird_wide$ENG_2[is.na(df_bird_wide$ENG_2)] <- 0
df_bird_wide$SRF_1[is.na(df_bird_wide$SRF_1)] <- 0
df_bird_wide$SRF_2[is.na(df_bird_wide$SRF_2)] <- 0

df_bird_wide <- df_bird_wide %>% 
  dplyr::select(date_r, bvg_char, ID_new, mean_temp, moon_illu, natural_cover_325, contag_landscape_325, tca_landscape_325, BRP_1, BRP_2, BWR_1, BWR_2, BNB_1, BNB_2, ENG_1, ENG_2, SRF_1, SRF_2) %>% 
    distinct()

df_bird_norm <- df_bird_wide %>% mutate_at(c(4:8), ~decostand(., method = "standardize") %>% as.vector(.)) %>% 
  droplevels()

sp <- vegdist(df_bird_norm[,9:18], method = "bray")

sp_data <- decostand(df_bird_norm[,9:18], method = "range")

permutest(betadisper(sp, df_bird_wide$natural_cover_325))
permutest(betadisper(sp, df_bird_wide$contag_landscape_325))
permutest(betadisper(sp, df_bird_wide$tca_landscape_325))

PERMANOVA <- adonis2(df_bird_wide[,9:18] ~ df_bird_wide$bvg_char + df_bird_wide$moon_illu + df_bird_wide$mean_temp + df_bird_wide$natural_cover_325 + df_bird_wide$contag_landscape_325 + df_bird_wide$tca_landscape_325, by = "terms")
PERMANOVA
AICc.PERMANOVA2(PERMANOVA)

PERMANOVA <- adonis2(df_bird_wide[,9:18] ~ df_bird_wide$bvg_char + df_bird_wide$mean_temp + df_bird_wide$natural_cover_325 + df_bird_wide$contag_landscape_325 + df_bird_wide$tca_landscape_325, by = "terms")
PERMANOVA
AICc.PERMANOVA2(PERMANOVA)



# calculate principal coordinates analysis (Bray-Curtis)
pcoa.meio.bray <- cmdscale(sp, k = 2, eig = T)


# extract axis positions for each site from cmdscale object and create a dataframe for plotting
pcoa.meio.bray.plotting <- as.data.frame(pcoa.meio.bray$points)
colnames(pcoa.meio.bray.plotting) <- c("axis_1", "axis_2")

pcoa.meio.bray.plotting$bvg <- df_bird_norm$bvg_char
pcoa.meio.bray.plotting$ID <- df_bird_norm$ID_new

envfit <- envfit(pcoa.meio.bray, env = df_bird_wide[,c(2, 4, 6:8)])

spp.scrs <- as.data.frame(scores(envfit, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))

spp.fct <- as.data.frame(scores(envfit, display = "factor"))
spp.fct <- cbind(spp.fct, Species = rownames(spp.fct)) %>% 
  mutate(enviro = case_when(Species == "bvg_chardry_rainforest" ~ "dry_rainforest",
                             Species == "bvg_chareuc_open_shruby_under" ~ "euc_open_shruby_under",
                             Species == "bvg_chareuc_wood_grassy_under" ~ "euc_wood_grassy_under",
                             Species == "bvg_chareuc_wood_shruby_under" ~ "euc_wood_shruby_under",
                             Species == "bvg_charmulga_wood_grass_forbs" ~ "mulga_wood_grass_forbs",
                             Species == "bvg_charsaltbush_shrub" ~ "saltbush_shrub",
                             Species == "bvg_charsubtropical_rainforest" ~ "subtropical_rainforest"))

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.meio.bray$eig[1]/(sum(pcoa.meio.bray$eig))

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.meio.bray$eig[2]/(sum(pcoa.meio.bray$eig))

(pcoa.meio.bray.plot <- ggplot() +
  geom_point(pcoa.meio.bray.plotting, mapping = aes(x = axis_1, y = axis_2, colour = ID), size = 3) +
  scale_colour_manual(values = c("#8dd3c7", "#ffed6f", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")) +
  theme_bw() + 
  xlab("PCoA 1 (15.21%)") +
  ylab("PCoA 2 (14.17%)") +
  annotate(geom = 'text', label = 'bray', x = Inf, y = -Inf, hjust = 1.15, vjust = -1) +
  geom_point(envfit, mapping = aes(x = Dim1, y = Dim2)) + 
  coord_fixed() +
  geom_segment(data = spp.scrs,
               aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_text(data = spp.scrs, aes(x = Dim1, y = Dim2, label = Species),
            size = 3, check_overlap = T) +
  geom_text(data = spp.fct, aes(x = Dim1, y = Dim2, label = enviro),
            size = 3, check_overlap = T))
ggsave(getDataPath("Figures", "27.05.2022_pcoa_birds.jpg"))

#Insects----

filtered <- filter(data_og, RFclass == "insect") %>% 
  droplevels()
filtered$date_r <- dmy(filtered$date_r)

ggplot(filtered, aes(x = ID, y = mean_temp, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_temp_insects.jpg"))

ggplot(filtered, aes(x = ID, y = natural_cover_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_naturalcover_insects.jpg"))

ggplot(filtered, aes(x = ID, y = contag_landscape_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_contag_insects.jpg"))

ggplot(filtered, aes(x = ID, y = tca_landscape_325, colour = bvg_char)) + 
  geom_point() + 
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "27.05.2022_tca_insects.jpg"))

ggplot(filtered, aes(x = ID, y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"))
ggsave(getDataPath("Figures", "23.05.2022_boxplot_enviro_insects.jpg"))


ggplot(data = filtered, aes(x = date_r, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_insect_bvg_time.jpg"))

filtered %>%
  ggplot(aes(x = mean_temp, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  facet_grid(.~ site)
ggsave(getDataPath("Figures", "27.05.2022_insect_veg_temp.jpg"))

ggplot(filtered, aes(x = moon_illu, y = n, colour = bvg_char)) +
  geom_point() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  geom_smooth(se = F, method = "loess", na.rm = T, span = 100) +
  facet_grid(.~site)
ggsave(getDataPath("Figures", "23.05.2022_insect_moon.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(natural_cover_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(name = "Vegetation groups", labels = c("Dry rainforest", "Eucalyptus open forest - shrubby understorey", "Eucalyptus woodlands - tussock grass understorey", "Eucalyptus woodland - shrubby understorey", "Mulga woodlands +/- tussock grass +/- forbs", "Saltbush and bluebush shrublands", "Sub-tropical rainforest"), values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  xlab("Natural cover - m2") +
  stat_n_text() +
  facet_grid(.~ site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_insect_naturalcover.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(contag_landscape_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(name = "Vegetation groups", labels = c("Dry rainforest", "Eucalyptus open forest - shrubby understorey", "Eucalyptus woodlands - tussock grass understorey", "Eucalyptus woodland - shrubby understorey", "Mulga woodlands +/- tussock grass +/- forbs", "Saltbush and bluebush shrublands", "Sub-tropical rainforest"), values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  xlab("Connectivity") +
  stat_n_text() +
  facet_grid(.~ site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_insect_contag.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(tca_landscape_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(name = "Vegetation groups", labels = c("Dry rainforest", "Eucalyptus open forest - shrubby understorey", "Eucalyptus woodlands - tussock grass understorey", "Eucalyptus woodland - shrubby understorey", "Mulga woodlands +/- tussock grass +/- forbs", "Saltbush and bluebush shrublands", "Sub-tropical rainforest"), values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221")) +
  xlab("Total core area") +
  stat_n_text() +
  facet_grid(.~site, scales = "free")
ggsave(getDataPath("Figures", "27.05.2022_insect_tca.jpg"))



#PERMANOVA ----

df_bird_wide <- pivot_wider(filtered, names_from = "ID", values_from = "n")
df_bird_wide$BRP_1[is.na(df_bird_wide$BRP_1)] <- 0
df_bird_wide$BRP_2[is.na(df_bird_wide$BRP_2)] <- 0
df_bird_wide$BWR_1[is.na(df_bird_wide$BWR_1)] <- 0
df_bird_wide$BWR_2[is.na(df_bird_wide$BWR_2)] <- 0
df_bird_wide$BNB_1[is.na(df_bird_wide$BNB_1)] <- 0
df_bird_wide$BNB_2[is.na(df_bird_wide$BNB_2)] <- 0
df_bird_wide$ENG_1[is.na(df_bird_wide$ENG_1)] <- 0
df_bird_wide$ENG_2[is.na(df_bird_wide$ENG_2)] <- 0
df_bird_wide$SRF_1[is.na(df_bird_wide$SRF_1)] <- 0
df_bird_wide$SRF_2[is.na(df_bird_wide$SRF_2)] <- 0

df_bird_wide <- df_bird_wide %>% 
  dplyr::select(date_r, bvg_char, ID_new, mean_temp, moon_illu, natural_cover_325, contag_landscape_325, tca_landscape_325, BRP_1, BRP_2, BWR_1, BWR_2, BNB_1, BNB_2, ENG_1, ENG_2, SRF_1, SRF_2) %>% 
  distinct()

df_bird_norm <- df_bird_wide %>% mutate_at(c(4:8), ~decostand(., method = "standardize") %>% as.vector(.)) %>% 
  droplevels()

sp <- vegdist(df_bird_norm[,9:18], method = "bray")

sp_data <- decostand(df_bird_norm[,9:18], method = "range")

permutest(betadisper(sp, df_bird_wide$natural_cover_325))
permutest(betadisper(sp, df_bird_wide$contag_landscape_325))
permutest(betadisper(sp, df_bird_wide$tca_landscape_325))

PERMANOVA <- adonis2(df_bird_wide[,9:18] ~ df_bird_wide$bvg_char + df_bird_wide$moon_illu + df_bird_wide$mean_temp + df_bird_wide$natural_cover_325 + df_bird_wide$contag_landscape_325 + df_bird_wide$tca_landscape_325, by = "terms")
PERMANOVA
AICc.PERMANOVA2(PERMANOVA)

PERMANOVA <- adonis2(df_bird_wide[,9:18] ~ df_bird_wide$bvg_char + df_bird_wide$mean_temp + df_bird_wide$natural_cover_325 + df_bird_wide$contag_landscape_325 + df_bird_wide$tca_landscape_325, by = "terms")
PERMANOVA
AICc.PERMANOVA2(PERMANOVA)



# calculate principal coordinates analysis (Bray-Curtis)
pcoa.meio.bray <- cmdscale(sp, k = 2, eig = T)


# extract axis positions for each site from cmdscale object and create a dataframe for plotting
pcoa.meio.bray.plotting <- as.data.frame(pcoa.meio.bray$points)
colnames(pcoa.meio.bray.plotting) <- c("axis_1", "axis_2")

pcoa.meio.bray.plotting$bvg <- df_bird_norm$bvg_char
pcoa.meio.bray.plotting$ID <- df_bird_norm$ID_new

envfit <- envfit(pcoa.meio.bray, env = df_bird_wide[,c(2, 4, 6:8)])

spp.scrs <- as.data.frame(scores(envfit, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))

spp.fct <- as.data.frame(scores(envfit, display = "factor"))
spp.fct <- cbind(spp.fct, Species = rownames(spp.fct)) %>% 
  mutate(enviro = case_when(Species == "bvg_chardry_rainforest" ~ "dry_rainforest",
                            Species == "bvg_chareuc_open_shruby_under" ~ "euc_open_shruby_under",
                            Species == "bvg_chareuc_wood_grassy_under" ~ "euc_wood_grassy_under",
                            Species == "bvg_chareuc_wood_shruby_under" ~ "euc_wood_shruby_under",
                            Species == "bvg_charmulga_wood_grass_forbs" ~ "mulga_wood_grass_forbs",
                            Species == "bvg_charsaltbush_shrub" ~ "saltbush_shrub",
                            Species == "bvg_charsubtropical_rainforest" ~ "subtropical_rainforest"))

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.meio.bray$eig[1]/(sum(pcoa.meio.bray$eig))

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.meio.bray$eig[2]/(sum(pcoa.meio.bray$eig))

(pcoa.meio.bray.plot <- ggplot() +
    geom_point(pcoa.meio.bray.plotting, mapping = aes(x = axis_1, y = axis_2, colour = ID), size = 3) +
    scale_colour_manual(values = c("#8dd3c7", "#ffed6f", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")) +
    theme_bw() + 
    xlab("PCoA 1 (20.12%)") +
    ylab("PCoA 2 (19.65%)") +
    annotate(geom = 'text', label = 'bray', x = Inf, y = -Inf, hjust = 1.15, vjust = -1) +
    geom_point(envfit, mapping = aes(x = Dim1, y = Dim2)) + 
    coord_fixed() +
    geom_segment(data = spp.scrs,
                 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
    geom_text(data = spp.scrs, aes(x = Dim1, y = Dim2, label = Species),
              size = 3, check_overlap = T) +
    geom_text(data = spp.fct, aes(x = Dim1, y = Dim2, label = enviro),
              size = 3, check_overlap = T))
ggsave(getDataPath("Figures", "27.05.2022_pcoa_insect.jpg"))


#Frogs----

#Plots ----

filtered <- filter(data_og, RFclass == "frog") %>% 
  droplevels()
filtered$date_r <- dmy(filtered$date_r)
summary(filtered)

ggplot(filtered, aes(x = ID, y = n, colour = bvg_char)) +
  geom_boxplot() +
scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) 
ggsave(getDataPath("Figures", "23.05.2022_boxplot_enviro_frogs.jpg"))

#Kruskal wallis and dunn ----

kruskal.test(filtered$n, filtered$ID)
dunn.test(filtered$n, filtered$ID, alpha = 0.05, altp = T)

ggplot(data = filtered, aes(x = date_r, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) +
  # scale_colour_manual(values = c("#c51b7d", "#4d9221", "#5ab4ac", "#91bfdb")) +
  facet_grid(.~site, scales = "free")
ggsave(getDataPath("Figures", "24.05.2022_frog_bvg_time.jpg"))

filtered %>%
  ggplot(aes(x = mean_temp, y = n, colour = bvg_char)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) +
  facet_grid(.~ site)
ggsave(getDataPath("Figures", "24.05.2022_frog_bvg_temp.jpg"))


ggplot(filtered, aes(x = moon_illu, y = n, colour = bvg_char)) +
  geom_point() +
  stat_smooth(se = F, method = "loess", na.rm = F, span = 100) +
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) +
  facet_grid(.~site)
ggsave(getDataPath("Figures", "23.05.2022_frog_moon.jpg"))


filtered %>%
  ggplot(aes(x = as.factor(round(natural_cover_3k)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) +
  # geom_smooth()
  facet_grid(.~ site, scales = "free")
ggsave(getDataPath("Figures", "24.05.2022_frog_naturalcover.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(np_landscape_3k)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) +
  # geom_smooth()
  facet_grid(.~ site, scales = "free")
ggsave(getDataPath("Figures", "24.05.2022_frog_np.jpg"))

filtered %>%
  ggplot(aes(x = as.factor(round(tca_landscape_325)), y = n, colour = bvg_char)) +
  geom_boxplot() +
  scale_colour_manual(values = c("#4d9221", "#35978f", "#35978f", "#35978f", "#abdda4", "#dfc27d", "#4d9221")) +
  # geom_smooth()
  facet_grid(.~site, scales = "free")
ggsave(getDataPath("Figures", "23.05.2022_frog_tca.jpg"))

filtered %>% pivot_longer(cols = c(mean_temp, natural_cover_3k, np_landscape_3k, tca_landscape_325), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = ID, y = value, colour = veg_type)) + 
  geom_point() + 
  facet_wrap(.~variable, scales = "free")
ggsave(getDataPath("Figures", "24.05.2022_landscapevars_point_frog.jpg"))

#PERMANOVA ----

df_frog_wide <- pivot_wider(filtered, names_from = "ID", values_from = "n")
df_frog_wide$BRP_1[is.na(df_frog_wide$BRP_1)] <- 0
df_frog_wide$BRP_2[is.na(df_frog_wide$BRP_2)] <- 0
df_frog_wide$BWR_2[is.na(df_frog_wide$BWR_2)] <- 0
df_frog_wide$BNB_1[is.na(df_frog_wide$BNB_1)] <- 0
df_frog_wide$ENG_2[is.na(df_frog_wide$ENG_2)] <- 0


df_frog_wide <- df_frog_wide %>% 
  dplyr::select(date_r, bvg_char, ID_new, mean_temp, moon_illu, natural_cover_3k, np_landscape_3k, tca_landscape_325, BRP_1, BRP_2, BNB_1, BWR_2, ENG_2) %>% 
  distinct()

df_frog_norm <- df_frog_wide %>% mutate_at(c(4:8), ~decostand(., method = "standardize") %>% as.vector(.)) %>% 
  droplevels()

sp <- vegdist(df_frog_norm[,9:13], method = "bray")

sp_data <- decostand(df_frog_norm[,9:13], method = "range")

permutest(betadisper(sp, df_frog_wide$bvg_char))
permutest(betadisper(sp, df_frog_wide$natural_cover_3k))
permutest(betadisper(sp, df_frog_wide$np_landscape_3k))
permutest(betadisper(sp, df_frog_wide$tca_landscape_325))

PERMANOVA <- adonis2(df_frog_wide[,9:13] ~ df_frog_wide$bvg_char + df_frog_wide$moon_illu + df_frog_wide$mean_temp + df_frog_wide$natural_cover_3k + df_frog_wide$np_landscape_3k + df_frog_wide$tca_landscape_325, by = "terms")
PERMANOVA
AICc.PERMANOVA2(PERMANOVA)

PERMANOVA <- adonis2(df_frog_wide[,9:13] ~ df_frog_wide$bvg_char + df_frog_wide$mean_temp + df_frog_wide$natural_cover_3k + df_frog_wide$np_landscape_3k + df_frog_wide$tca_landscape_325, by = "terms")
PERMANOVA
AICc.PERMANOVA2(PERMANOVA)

# calculate principal coordinates analysis (Bray-Curtis)
pcoa.meio.bray <- cmdscale(sp, k = 2, eig = T)


# extract axis positions for each site from cmdscale object and create a dataframe for plotting
pcoa.meio.bray.plotting <- as.data.frame(pcoa.meio.bray$points)
colnames(pcoa.meio.bray.plotting) <- c("axis_1", "axis_2")

pcoa.meio.bray.plotting$bvg <- df_frog_norm$bvg_char
pcoa.meio.bray.plotting$ID <- df_frog_norm$ID_new

envfit <- envfit(pcoa.meio.bray, env = df_frog_wide[,c(2, 4, 6:8)])

spp.scrs <- as.data.frame(scores(envfit, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))

spp.fct <- as.data.frame(scores(envfit, display = "factor"))
spp.fct <- cbind(spp.fct, Species = rownames(spp.fct))

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.meio.bray$eig[1]/(sum(pcoa.meio.bray$eig))

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.meio.bray$eig[2]/(sum(pcoa.meio.bray$eig))

(pcoa.meio.bray.plot <- ggplot() +
    geom_point(pcoa.meio.bray.plotting, mapping = aes(x = axis_1, y = axis_2, colour = ID), size = 3) +
    scale_colour_manual(values = c("#8c510a", "#35978f", "#01665e", "#fdae61", "#762a83")) +
    theme_bw() + 
    xlab("PCoA 1 (53.85%)") +
    ylab("PCoA 2 (16.80%)") +
    annotate(geom = 'text', label = 'bray', x = Inf, y = -Inf, hjust = 1.15, vjust = -1) +
    geom_point(envfit, mapping = aes(x = Dim1, y = Dim2)) + 
    coord_fixed() +
    geom_segment(data = spp.scrs,
                 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
    geom_text(data = spp.scrs, aes(x = Dim1, y = Dim2, label = Species),
              size = 3, check_overlap = T) +
    geom_text(data = spp.fct, aes(x = Dim1, y = Dim2, label = Species),
              size = 3, check_overlap = T))
ggsave(getDataPath("Figures", "24.05.2022_pcoa_frog.jpg"))
