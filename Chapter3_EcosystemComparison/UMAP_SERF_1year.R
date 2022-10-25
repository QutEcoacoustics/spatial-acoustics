library(ggplot2)
library(tidyverse)

rm(list=ls())

data_wavelets <- list.files('C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/AIndices/10_RF/Round1', pattern = glob2rx("*chp4.csv_class_RFlabels.csv"), full.names = T, recursive = T)



df <- read.csv(data_wavelets) 




data <- df %>% 
  separate(X, into = c("site", "point", "batch", "index", "motif_id", "motif_what"), remove = F) %>% 
  mutate(., anthrophony = case_when(RFclass == "anthro" ~ "yes",
    RFclass == "anthrobird" ~ "yes",
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
                                RFclass == "insectgeo" ~ "insect",
                                RFclass == "birdback" ~ "bird",
                                RFclass == "geobat" ~ "bat",
                                RFclass == "anthrofrog" ~ "frog",
                                RFclass == "insectfroggeo" ~ "insectfrog",
                                RFclass == "zerobird" ~ "bird",
                                RFclass == "birdfroggeo" ~ "birdfrog",
                                RFclass == "froggeo" ~ "frog",
                                RFclass == "zerogeobird" ~ "bird",
                                RFclass == "bgn" ~ "geo",
                                RFclass == "zerogeo" ~ "geo",
                                TRUE ~ as.character(RFclass)
  )) %>% 
  filter(RFclass != "birdfroginsect" | RFclass != "froginsect" | RFclass != "geobird")

data %>% group_by(RFclass) %>% 
  count()

library(umap)


data.df <- data[,c(28:111)] 
data.df[is.na(data.df)] <- 0


data.label <- data[,c(4,5,9, 24, 26)] %>% 
  mutate("ID2" = row_number()) %>% 
  mutate(season = case_when(batch == "202012" ~ "hot", 
                            batch == "202001" ~ "hot",
                            batch == "202002" ~ "hot",
                            batch == "202003" ~ "cold",
                            batch == "202004" ~ "cold",
                            batch == "202005" ~ "cold",
                            batch == "202006" ~ "cold",
                            batch == "202007" ~ "cold",
                            batch == "202008" ~ "cold",
                            batch == "202009" ~ "hot",
                            batch == "202010" ~ "hot",
                            batch == "202011" ~ "hot"))

umap_test <- umap(data.df)

umap_df <- umap_test$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID2=row_number())%>%
  inner_join(data.label, by = "ID2")

umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = RFclass)) +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot") +
  facet_wrap(.~batch)

ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Figures/umap/UMAP_SERF_monthly.png")
