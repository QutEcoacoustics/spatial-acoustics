library(ggplot2)
library(tidyverse)

rm(list=ls())

data_wavelets <- list.files('C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/', pattern = glob2rx("*class_RFlabels.csv"), full.names = T, recursive = T)



df <- read.csv(data_wavelets[1]) 



for (file in data_wavelets[2:22]) {
  df1 <- read.csv(file)
  
  
  df <- plyr::rbind.fill(df1, df)
  
}

data <- df %>% 
  separate(id, into = c("site.ID", "point.ID", "batch.ID", "index", "motif_id", "motif_what"), remove = F)

rm(df1)

df3 <- read.csv(data_wavelets[23]) %>% 
  mutate(batch.ID = "bowraaug") %>% 
  rename("point.ID" = point) %>% 
  mutate(site.ID = 65) %>% 
  select(batch.ID, site.ID, everything(), -X)

data_all <- plyr::rbind.fill(data, df3)

df4 <- read.csv(data_wavelets[24]) %>% 
  mutate(batch.ID = "bowraoct") %>% 
  separate(id, into = c("point.ID", "index", "motif_id", "motif_what"), remove = F) %>% 
  mutate(site.ID = 65) %>%
  select(batch.ID, site.ID, everything(), -X)

data_all <- plyr::rbind.fill(data_all, df4)

df5 <- read.csv(data_wavelets[25]) %>% 
  mutate(batch.ID = "SERF_TERN") %>% 
  rename("month" = id) %>% 
  rename("index" = X) %>% 
  rename("motif_id" = X.1) %>% 
  rename("motif_what" = X.2) %>% 
  mutate("point.ID" = 253) %>% 
  mutate("site.ID" = 64) %>% 
  select(batch.ID, point.ID, site.ID, everything())

data_all <- plyr::rbind.fill(data_all, df5) %>% 
  select(site.ID, point.ID, batch.ID, X, classID, component, component_model, class_model, index_name, number, what, accuracy, everything())

data.df <- data_all[,c(40:209)] %>% 
  select(everything(), -component_model1)

data.df[is.na(data.df)] <- 0

summary(data.df)

data.label <- data_all[,c(1,2,3,15,16)] %>% 
  mutate(ID2=row_number())

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
       subtitle = "UMAP plot") 

ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Figures/umap/UMAP_allserf1.png")

data_serf <- filter(data_all, batch.ID == "SERF_TERN" | site.ID == "64")

library(umap)


data.df <- data_serf[,c(40:209)] %>% 
  select(everything(), -component_model1)

data.df[is.na(data.df)] <- 0

summary(data.df)

data.label <- data_serf[,c(2,3,4)] %>% 
  mutate(ID2=row_number())

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
             colour = batch.ID,
             shape = point.ID)) +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot")

ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Figures/umap/UMAP_allserf1.png")

data_serf <- filter(data_all, point.ID == "253" & batch.ID != "SERF_TERN" & X != "NA")

library(umap)


data.df <- data_serf[,c(40:209)] %>% 
  select(everything(), -component_model1)
data.df[is.na(data.df)] <- 0

summary(data.df)

data.label <- data_serf[,c(2,3,15)] %>% 
  mutate(ID2=row_number())

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
       subtitle = "UMAP plot")

ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Figures/umap/UMAP_allserf1.png")


data_bowra <- filter(data_all, point.ID == "WA001" | point.ID == "WA002" | point.ID == "WA003" | point.ID == "WA004" | point.ID == "WA005" | point.ID == "WA006" | point.ID == "WA007" | point.ID == "WA008" | point.ID == "WA009" | point.ID == "WA010" | point.ID == "WA011" | point.ID == "WA012" | point.ID == "WA013" | point.ID == "WA014" | point.ID == "WA015" | point.ID == "WA016" | point.ID == "WA017" | point.ID == "WA018" | point.ID == "WA019" | point.ID == "WA020" | point.ID == "WA021" | point.ID == "WA022" | point.ID == "WA023" | point.ID == "WA024" | point.ID == "258") %>% 
  mutate(point.ID = case_when(point.ID == "WA024" ~ "WA001",
                              point.ID == "WA023" ~ "WA002",
                              point.ID == "WA022" ~ "WA003",
                              point.ID == "WA021" ~ "WA004",
                              point.ID == "WA020" ~ "WA005",
                              point.ID == "WA019" ~ "WA006",
                              point.ID == "WA018" ~ "WA007",
                              point.ID == "WA017" ~ "WA008",
                              point.ID == "WA016" ~ "WA009",
                              point.ID == "WA015" ~ "WA010",
                              point.ID == "WA014" ~ "WA011",
                              point.ID == "WA013" ~ "WA012",
                            TRUE ~ point.ID)) %>% 
  filter(point.ID == "WA012" | point.ID == "WA001" | point.ID == "258")

data.df <- data_bowra[,c(40:209)] %>% 
  select(everything(), -component_model1)
data.df[is.na(data.df)] <- 0

summary(data.df)

data.label <- data_bowra[,c(1,2,3)] %>% 
  mutate(ID2=row_number())

umap_bowra <- umap(data.df)

umap_df_bowra <- umap_bowra$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID2=row_number())%>%
  inner_join(data.label, by = "ID2")

umap_df_bowra %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             colour = point.ID)) +
  geom_point() +
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot")

ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/Figures/umap/UMAP_allbowra_bybatch.png")

