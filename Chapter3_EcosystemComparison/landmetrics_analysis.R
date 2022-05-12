#Tidying landscape metrics ----

rm(list = ls())

largerbuffer_files <- list.files('D:/Chapter3_EcosystemComparison/6_LandscapeMetrics', pattern = glob2rx("*3k.csv"), full.names = T, recursive = T)

read_in <- lapply(largerbuffer_files, read.csv)

final_df <- rbind(read_in[[1]], read_in[[2]], read_in[[3]], read_in[[4]], read_in[[5]], read_in[[6]], read_in[[7]], read_in[[8]], read_in[[9]], read_in[[10]])

smallerbuffer_files <- list.files('D:/Chapter3_EcosystemComparison/6_LandscapeMetrics', pattern = glob2rx("*325.csv"), full.names = T, recursive = T)

read_in <- lapply(smallerbuffer_files, read.csv)

final_df2 <- rbind(read_in[[1]], read_in[[2]], read_in[[3]], read_in[[4]], read_in[[5]], read_in[[6]], read_in[[7]], read_in[[8]], read_in[[9]], read_in[[10]])

df <- rbind(final_df, final_df2)

split <- split(df, df$level)

land_metrics <- split$landscape %>% 
  select(everything(), -c("class", "id")) %>% 
  mutate(land_metric = paste(level, metric, sep = "_")) %>% 
  filter(land_metric == "landscape_contag") %>% 
  pivot_wider(names_from = land_metric, values_from = value)


class_metrics <- split$class %>% 
  mutate(land_metric = paste(level, metric, sep = "_")) %>%
  filter(land_metric == "class_np" | land_metric == "class_ca") %>% 
  mutate(land_metric = paste(land_metric, class, sep = "_")) %>% 
  select(everything(), -c("level", "metric", "id", "class")) %>% 
  pivot_wider(names_from = land_metric, values_from = value)



patch_metrics <- split$patch %>% 
  mutate(land_metric = paste(level, metric, sep = "_")) %>%
  filter(land_metric == "patch_area" | land_metric == "patch_ncore") %>% 
  mutate(land_metric = paste(land_metric, class, sep = "_")) %>% 
  select(everything(), -c("level", "metric", "class"))



library(landscapemetrics)
lsm <- lsm_abbreviations_names %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/landscapemetrics_names.csv")





# cor(land_metrics[,8:89]) %>% write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/correlation_land.csv")
# cor(class_metrics[,8:89]) %>% write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/08.04.2022_correlation_class.csv")
# cor(patch_metrics[,8:89]) %>% write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter3_SoundscapeEcosystemComparation/correlation_patch.csv")



df <- filter(df, metric == "np" | metric == "contag" | metric == "ca" | metric == "pr" | metric == "tca" | metric == "area" | metric == "ncore") %>% 
  mutate(size = case_when(buffer_size == "dry3k" ~ "3k",
                          buffer_size == "wet3k" ~ "3k",
                          TRUE ~ "325")) %>% 
  mutate(point = case_when(buffer_size == "dry3k" ~ "drya",
                           buffer_size == "wet3k" ~ "weta",
                           buffer_size == "dry325" ~ "drya",
                           buffer_size == "wet325" ~ "weta"))

split <- split(df, df$level)

land_metrics <- split$landscape
class_metrics <- split$class
patch_metrics <- split$patch

#Landscape ----


wider_land <- pivot_wider(land_metrics, names_from = metric, values_from = value)

wider_land_325 <- filter(wider_land, size == "325") 
wider_land_3k <- filter(wider_land, size == "3k")

cor.test(wider_land_325$np, wider_land_325$contag)
cor.test(wider_land_325$np, wider_land_325$pr)
cor.test(wider_land_325$np, wider_land_325$tca)

cor.test(wider_land_325$contag, wider_land_325$pr)
cor.test(wider_land_325$contag, wider_land_325$np)
cor.test(wider_land_325$contag, wider_land_325$tca)

cor.test(wider_land_325$np, wider_land_325$tca)



cor.test(wider_land_3k$np, wider_land_3k$contag)
cor.test(wider_land_3k$np, wider_land_3k$pr) #ok
cor.test(wider_land_3k$np, wider_land_3k$tca)

cor.test(wider_land_3k$contag, wider_land_3k$pr)
cor.test(wider_land_3k$contag, wider_land_3k$np)
cor.test(wider_land_3k$contag, wider_land_3k$tca)

cor.test(wider_land_3k$np, wider_land_3k$tca)

#class ----


wider_class <- pivot_wider(class_metrics, names_from = metric, values_from = value)

wider_class_325 <- filter(wider_class, size == "325") 
wider_class_3k <- filter(wider_class, size == "3k")

cor.test(wider_class_325$np, wider_class_325$ca) #ok
cor.test(wider_class_325$np, wider_class_325$tca) #ok

cor.test(wider_class_325$ca, wider_class_325$tca)

cor.test(wider_class_3k$np, wider_class_3k$ca) #ok
cor.test(wider_class_3k$np, wider_class_3k$tca) #ok

cor.test(wider_class_3k$ca, wider_class_3k$tca) #no


#patch ----

patch_metrics <- split$patch


wider_patch <- pivot_wider(patch_metrics, names_from = metric, values_from = value)

wider_patch_325 <- filter(wider_patch, size == "325") 
wider_patch_3k <- filter(wider_patch, size == "3k")


cor.test(wider_patch_3k$area, wider_patch_3k$ncore) #ok

cor.test(wider_patch_325$area, wider_patch_325$ncore) #ok
