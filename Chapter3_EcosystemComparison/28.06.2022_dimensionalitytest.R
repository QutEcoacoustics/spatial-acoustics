library(ggplot2)
library(tidyverse)

rm(list=ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation",  ...))
}

data_wavelets <- list.files('C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/AIndices/10_RF/Round1', pattern = glob2rx("*chp3.csv_class_RFlabels.csv"), full.names = T, recursive = T)



df <- read.csv(data_wavelets[1]) 



for (file in data_wavelets[2:10]) {
  df1 <- read.csv(file)
  
  
  df <- plyr::rbind.fill(df1, df)
  
}

data <- df %>% 
  separate(X, into = c("site", "point", "batch", "index", "motif_id", "motif_what"), remove = F)

rm(df1)

data <- mutate(data, anthrophony = case_when(RFclass == "anthrobird" ~ "yes",
                                             RFclass == "anthrobirdfroggeoinsect" ~ "yes",
                                             RFclass == "anthrobirdgeo" ~ "yes",
                                             RFclass == "anthrobirdgeoinsect" ~ "yes",
                                             RFclass == "anthrobirdinsect" ~ "yes",
                                             RFclass == "anthrofroggeoinsect" ~ "yes",
                                             RFclass == "anthrofroginsect" ~ "yes",
                                             RFclass == "anthrogeoinsect" ~ "yes",
                                             RFclass == "anthroinsect" ~ "yes",
                                             RFclass == "anthrogeo" ~ "yes",
                                             RFclass == "anthrofrog" ~ "yes",
                                             RFclass == "anthrogeo" ~ "yes",
                                             TRUE ~ "no")) %>% 
  mutate(., geophony = case_when(RFclass == "anthrobirdfroggeoinsect" ~ "yes",
                                 RFclass == "anthrobirdgeo" ~ "yes",
                                 RFclass == "anthrobirdgeoinsect" ~ "yes",
                                 RFclass == "anthrofroggeoinsect" ~ "yes",
                                 RFclass == "anthrogeoinsect" ~ "yes",
                                 RFclass == "birdgeo" ~ "yes",
                                 RFclass == "birdgeoinsect" ~ "yes",
                                 RFclass == "froggeoinsect" ~ "yes",
                                 RFclass == "geoinsect" ~ "yes",
                                 RFclass == "insectgeo" ~ "yes",
                                 RFclass == "bgn" ~ "yes",
                                 RFclass == "geo" ~ "yes",
                                 RFclass == "zeroback" ~ "yes",
                                 RFclass == "zerogeo" ~ "yes",
                                 RFclass == "anthrogeo" ~ "yes",
                                 RFclass == "backgeo" ~ "yes",
                                 RFclass == "geobat" ~ "yes",
                                 RFclass == "insectfroggeo" ~ "yes",
                                 RFclass == "anthrogeo" ~ "yes",
                                 RFclass == "backgeo" ~ "yes",
                                 RFclass == "birdfroggeo" ~ "yes",
                                 RFclass == "froggeo" ~ "yes",
                                 RFclass == "zerogeobird" ~ "frog",
                                 TRUE ~ "no")) %>% 
  mutate(., RFclass = case_when(RFclass == "anthrobird" ~ "bird",
                                RFclass == "anthrogeo" ~ "anthro",
                                RFclass == "anthrobirdfroggeoinsect" ~ "birdfroginsect",
                                RFclass == "anthrobirdgeo" ~ "bird",
                                RFclass == "anthrobirdgeoinsect" ~ "insect",
                                RFclass == "anthrobirdinsect" ~ "insect",
                                RFclass == "anthrofroggeoinsect" ~ "froginsect",
                                RFclass == "anthrofroginsect" ~ "froginsect",
                                RFclass == "anthrogeoinsect" ~ "insect",
                                RFclass == "anthroinsect" ~ "insect",
                                RFclass == "birdgeo" ~ "bird",
                                RFclass == "birdgeoinsect" ~ "insect",
                                RFclass == "froggeoinsect" ~ "froginsect",
                                RFclass == "geoinsect" ~ "insect",
                                RFclass == "insectgeo" ~ "insect",
                                RFclass == "birdback" ~ "bird",
                                RFclass == "geobat" ~ "bat",
                                RFclass == "anthrofrog" ~ "frog",
                                RFclass == "insectfroggeo" ~ "froginsect",
                                RFclass == "zerobird" ~ "bird",
                                RFclass == "birdfroggeo" ~ "birdfrog",
                                RFclass == "froggeo" ~ "frog",
                                RFclass == "zerogeobird" ~ "bird",
                                RFclass == "banjobird" ~ "birdfrog",
                                RFclass == "anthrobirdfrog" ~ "birdfrog",
                                RFclass == "anthrobirdfroggeo" ~ "birdfrog",
                                RFclass == "anthrobirdfroginsect" ~ "birdfroginsect",
                                RFclass == "backgeo" ~ "geo",
                                RFclass == "anthrofroggeo" ~ "frog",
                                RFclass == "zero" ~ "geo",
                                RFclass == "zeroback" ~ "geo",
                                RFclass == "zerogeo" ~ "geo",
                                RFclass == "bgn" ~ "geo",
                                RFclass == "insectfrog" ~ "froginsect",
                                RFclass == "birdfrog" ~ "frog",
                                RFclass == "anthrobatfroggeoinsect" ~ "frog",
                                RFclass == "anthrobirdgeomammal" ~ "bird",
                                RFclass == "froginsectmammal" ~ "frog",
                                RFclass == "frogbird" ~ "frog",
                                RFclass == "batbirdgeoinsect" ~"insect",
                                RFclass == "birdfroginsect" ~ "insect",
                                RFclass == "birdinsect" ~ "insect",
                                RFclass == "froginsect" ~ "frog",
                                RFclass == "birdfrog" ~ "frog",
                                RFclass == "birdfroginsect" ~ "frog",
                                TRUE ~ as.character(RFclass)
  )) %>% 
  mutate(ID = case_when(point == 305 ~ "BRP_1",
                        point == 306 ~ "BRP_2",
                        point == 6 ~ "BNB_1",
                        point == 7 ~ "BNB_2",
                        point == 259 ~ "BWR_1",
                        point == 258 ~ "BWR_2",
                        point == 110 ~ "ENG_1",
                        point == 111 ~ "ENG_2",
                        point == 253 ~ "SRF_1",
                        point == 254 ~ "SRF_2")) %>% 
  mutate(bvg_char = case_when(point == 305 ~ "euc_woodland/tussockgrass",
                              point == 306 ~ "euc_woodland/shrubbyunder",
                              point == 6 ~ "saltbush",
                              point == 7 ~ "saltbush",
                              point == 259 ~ "mulga_wood",
                              point == 258 ~ "euc_woodland/tussockgrass",
                              point == 110 ~ "euc_openforest",
                              point == 111 ~ "dry_rainforest",
                              point == 253 ~ "euc_woodland/tussockgrass",
                              point == 254 ~ "subtropical_rainforest")) %>% 
  select(ID, anthrophony, geophony, bvg_char, everything()) %>% 
  filter(RFclass != "bat" & RFclass != "anthro") %>% 
  droplevels(.)

library(umap)
data %>% group_by(RFclass) %>% 
  count()

data.df <- data[,c(31:137)] %>% 
  select(everything(), -FileName_end)
data.df[is.na(data.df)] <- 0


data.label <- data[,c(1,4,13, 8, 29, 9)] %>% 
  mutate(ID2=row_number())

set.seed(12)
umap_test <- umap(data.df, preserve.seed = TRUE)

umap_df <- umap_test$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID2=row_number())%>%
  inner_join(data.label, by = "ID2")

bvg_labs <- c("Dry rainforest", "Euc. open forest/shrub understorey", "Euc. woodlands/grass understorey", "Euc. woodland/shrub understorey", "Mulga woodlands/grass/forbs", "Saltbush/bluebush shrublands", "Sub-tropical rainforest")
names(bvg_labs) <- c("dry_rainforest", "euc_openforest", "euc_woodland/tussockgrass", "euc_woodland/shrubbyunder", "mulga_wood", "saltbush", "subtropical_rainforest")

labs <- c("Bird", "Frog", "Insect", "Geophony")
names(labs) <- c("bird", "frog", "insect", "geo")

month_labs <- c("August", "September", "October")
names(month_labs) <- c("202008", "202009", "202010")

#figure in text ----
umap_df %>% filter(RFclass != "geo") %>% 
  group_by(ID, batch) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = ID)) +
  scale_colour_manual(values = c("#669900", "#99cc33", "#ccee66", "#006699", "#3399cc", "#990066", "#cc3399", "#ff6600", "#ff9900", "#ffcc00"), labels = c("BonBon1", "BonBon2", "Booroopki1", "Booroopki2", "Bowra1", "Bowra2", "Eungella1", "Eungella2", "SERF1", "SERF2"), name = "Recording location") +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2") +
  theme(text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(.~RFclass+batch, labeller = labeller(RFclass = labs, batch = month_labs), ncol = 3)
ggsave(getDataPath("Figures", "11.07.2022_umap_classesid.tiff"), height = 10, width = 20)

umap_df %>% filter(RFclass != "geo") %>% 
  group_by(ID, batch) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = bvg_char)) +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"), labels = bvg_labs, name = "Vegetation type") +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2") +
  theme(text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(.~RFclass+batch, labeller = labeller(RFclass = labs, batch = month_labs), ncol = 3)
ggsave(getDataPath("Figures", "11.07.2022_umap_classesbvg.tiff"), height = 10, width = 20)

#figure in text ----
umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = bvg_char)) +
  geom_point() +
  scale_colour_manual(values = c("#66c2a4", "#a6bddb", "#3690c0", "#023858", "#88419d", "#dfc27d", "#4d9221"), labels = bvg_labs, name = "Vegetation type") +
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 14)) +
  labs(x = "UMAP1",
       y = "UMAP2") +
  theme(text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) 
ggsave(getDataPath("Figures", "11.07.2022_umap_bvg.tiff"), height = 10, width = 20)

umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = ID)) +
  geom_point() +
  scale_colour_manual(values = c("#669900", "#99cc33", "#ccee66", "#006699", "#3399cc", "#990066", "#cc3399", "#ff6600", "#ff9900", "#ffcc00")) +
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 14), legend.key = element_rect(size = 10)) +
  labs(x = "UMAP1",
       y = "UMAP2")
ggsave(getDataPath("Figures", "11.07.2022_umap_id.jpg"), height = 10, width = 20)

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda

umap_df %>% filter(RFclass != "anthro") %>% 
  group_by(ID, batch) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = RFclass)) +
  scale_colour_manual(values = c("bird" = "#c51b7d", "insect" = "#5ab4ac", "frog" = "#f7fcb9", "birdinsect" = "#e9a3c9", "froginsect" = "#4d9221", "geo" = "#756bb1")) +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot")+
  facet_wrap(bvg_char~ID+batch+n, scales = "free")
ggsave(getDataPath("Figures", "07.07.2022_umap_time.jpg"), height = 10, width = 20)

umap_df %>% filter(RFclass != "anthro") %>% 
  group_by(ID, batch) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = RFclass)) +
  scale_colour_manual(values = c("bird" = "#c51b7d", "insect" = "#5ab4ac", "frog" = "#f7fcb9", "birdinsect" = "#e9a3c9", "froginsect" = "#4d9221", "geo" = "#756bb1")) +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2")+
  facet_wrap(.~batch, scales = "free")
ggsave(getDataPath("Figures", "07.07.2022_umap_timeall.jpg"), height = 10, width = 20)
  
ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Figures/umap/UMAP_a2o_faunavegmonth.png")
