library(ggplot2)
library(tidyverse)
library(Rcpp)
library(suncalc)
library(lubridate)

rm(list=ls())

data_wavelets <- list.files('C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/AIndices/10_RF/Round1', pattern = glob2rx("*chp4.csv_class_RFlabels.csv"), full.names = T, recursive = T)



df <- read.csv(data_wavelets) 






data <- df2 %>% 
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
                                 RFclass == "zerogeobird" ~ "yes",
                                 TRUE ~ "no")) %>% 
  mutate(., RFclass = case_when(RFclass == "anthrobird" ~ "bird",
                                RFclass == "anthrobirdfroggeoinsect" ~ "frog",
                                RFclass == "anthrobirdgeo" ~ "bird",
                                RFclass == "anthrobirdgeoinsect" ~ "birdinsect",
                                RFclass == "anthrobirdinsect" ~ "birdinsect",
                                RFclass == "anthrofroggeoinsect" ~ "frog",
                                RFclass == "anthrofroginsect" ~ "frog",
                                RFclass == "anthrogeoinsect" ~ "insect",
                                RFclass == "anthroinsect" ~ "insect",
                                RFclass == "birdgeo" ~ "bird",
                                RFclass == "birdgeoinsect" ~ "birdinsect",
                                RFclass == "froggeoinsect" ~ "frog",
                                RFclass == "geoinsect" ~ "insect",
                                RFclass == "insectgeo" ~ "insect",
                                RFclass == "birdback" ~ "bird",
                                RFclass == "geobat" ~ "bat",
                                RFclass == "anthrofrog" ~ "frog",
                                RFclass == "insectfroggeo" ~ "frog",
                                RFclass == "zerobird" ~ "bird",
                                RFclass == "birdfroggeo" ~ "frog",
                                RFclass == "froggeo" ~ "frog",
                                RFclass == "zerogeobird" ~ "bird",
                                RFclass == "bgn" ~ "geo",
                                RFclass == "zerogeo" ~ "geo",
                                RFclass == "froginsect" ~ "frog",
                                RFclass == "geobird" ~ "bird",
                                RFclass == "anthrogeo" ~ "anthro",
                                TRUE ~ as.character(RFclass)
  )) %>% 
  filter(RFclass != "frog")

data %>% group_by(RFclass) %>% 
  count()

library(umap)


data.df <- data[,c(48:131)] 
data.df[is.na(data.df)] <- 0


data.label <- data[,c(20, 22, 23, 24, 27, 28, 40, 133, 134, 135)] %>% 
  mutate("ID2" = row_number()) %>% 
  mutate(season = case_when(month == "202012" ~ "hot", 
                            month == "202001" ~ "hot",
                            month == "202002" ~ "hot",
                            month == "202003" ~ "cold",
                            month == "202004" ~ "cold",
                            month == "202005" ~ "cold",
                            month == "202006" ~ "cold",
                            month == "202007" ~ "cold",
                            month == "202008" ~ "cold",
                            month == "202009" ~ "hot",
                            month == "202010" ~ "hot",
                            month == "202011" ~ "hot"))
custom_config <- umap.defaults
custom_config$n_neighbors <- 5
custom_config$min_dist <- 0.0001
custom_config$metric <- "euclidean"


umap_test <- umap(data.df, custom_config)

umap_df <- umap_test$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID2=row_number())%>%
  inner_join(data.label, by = "ID2")

plot_info <- filter(data, RFclass != "NA" & month != "NA") %>% 
  distinct(batch, date) %>% 
  group_by(batch) %>% 
  summarise(n()) %>% 
  rename("n_days" = `n()`)

plot_info1 <- filter(data, RFclass != "NA" & month != "NA") %>% 
  left_join(., plot_info) %>% 
  group_by(batch) %>% 
  select(RFclass, date, n_days, batch) %>% 
  na.exclude() %>% 
  group_by(batch) %>% 
  mutate(n_motif =  n()) %>%
  mutate(avg_motif = paste("Motif/day=", format(round(n_motif/n_days, 0)))) %>% 
  mutate(n_motif_char = paste("n motifs=", n_motif, sep = "")) %>%
  mutate(n_days_char = paste("Recorded days=", n_days, sep = "")) %>%
  # mutate(label = case_when(batch = "202001" ~ paste("N motif:", n_motif, "=", n_motif, sep = " "))
droplevels()



labels <- select(plot_info1, batch, n_motif, n_days) %>% 
  distinct() %>% 
  mutate(month = case_when(batch == 202001 ~ "January",
                           batch == 202002 ~ "February",
                           batch == 202003 ~ "March",
                           batch == 202004 ~ "April",
                           batch == 202005 ~ "May",
                           batch == 202006 ~ "June",
                           batch == 202007 ~ "July",
                           batch == 202008 ~ "August",
                           batch == 202009 ~ "September",
                           batch == 202010 ~ "October",
                           batch == 202011 ~ "November",
                           batch == 202012 ~ "December")) %>% 
  mutate(average_motif = paste("Motif/day = ", format(round(n_motif/n_days)), sep = "")) %>% 
  mutate(days = paste("n days = ", n_days, sep = "")) %>% 
  mutate(labels = paste(month, average_motif, days, sep = " \n "))

labels$labels <- factor(labels$labels, levels = c("January \n Motif/day = 25 \n n days = 31", "February \n Motif/day = 25 \n n days = 29", "March \n Motif/day = 31 \n n days = 31", "April \n Motif/day = 23 \n n days = 22", "May \n Motif/day = 26 \n n days = 26", "June \n Motif/day = 33 \n n days = 30", "July \n Motif/day = 32 \n n days = 31", "August \n Motif/day = 31 \n n days = 31", "September \n Motif/day = 32 \n n days = 30", "October \n Motif/day = 32 \n n days = 31", "November \n Motif/day = 34 \n n days = 30", "December \n Motif/day = 19 \n n days = 31"))

month_labels <- labels$labels
names(month_labels) <- c("202001", "202002", "202003", "202005", "202006", "202007", "202008", "202009", "202010", "202012",  "202011", "202004")

data %>% group_by(batch) %>% 
count()

umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = RFclass)) +
  scale_colour_manual(values = c("#fc8d59", "#c51b7d", "#e9a3c9", "#3690c0", "#5ab4ac"), labels = c("Technophony", "Bird", "Bird/insect", "Geophony", "Insect"), name = "Sound class") +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2") +
  theme(strip.text.x = element_text(size = 13), panel.background = element_rect(inherit.blank = "white"), legend.text = element_text(size = 13), legend.title = element_text(size = 14)) +
  facet_wrap(.~month, labeller = labeller(batch = month_labels))

# ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment/Figures/GoodFigs/Fig2.tiff")

umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = RFclass)) +
  # scale_colour_manual(values = c("#fc8d59", "#c51b7d", "#e9a3c9", "#3690c0", "#5ab4ac"), labels = c("Technophony", "Bird", "Bird/insect", "Geophony", "Insect"), name = "Sound class") +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2") +
  theme(strip.text.x = element_text(size = 13), panel.background = element_rect(inherit.blank = "white"), legend.text = element_text(size = 13), legend.title = element_text(size = 14)) +
  facet_wrap(.~anthrophony, labeller = labeller(batch = month_labels))

graphical_abs_labels <- labels$month
names(graphical_abs_labels) <- c("202001", "202002", "202003", "202005", "202006", "202007", "202008", "202009", "202010", "202012",  "202011", "202004")

umap_df %>% filter(batch == "202001" | batch == "202004" | batch == "202007" | batch == "202010") %>% 
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = RFclass)) +
  scale_colour_manual(values = c("#fc8d59", "#c51b7d", "#e9a3c9", "#3690c0", "#5ab4ac"), labels = c("Technophony", "Bird", "Bird/insect", "Geophony", "Insect"), name = "Sound class") +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2") +
  theme(strip.text.x = element_text(size = 13), panel.background = element_rect(inherit.blank = "white"), legend.text = element_text(size = 13), legend.title = element_text(size = 14), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  facet_wrap(.~batch, labeller = labeller(batch = graphical_abs_labels), nrow = 1)

ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment/graphical_abstract/graphical_abstract_v2.tiff", height = 3, width = 11)

#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221
#birdfroginsect ##9ebcda


