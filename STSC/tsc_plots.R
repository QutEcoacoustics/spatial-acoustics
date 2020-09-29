library(ggplot2)
library(tidyverse)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}


cluster_results <- read.csv(getDataPath("STSC", "Results", "partitional5.csv")) %>%
  group_by(., id) %>% 
  mutate(., new_position = order(order(position))) %>% 
  ungroup(.) %>%
  separate(id, into = c("point1", "index_name", "motif_number", "what")) %>% 
  select(everything(), -point1)


colours <- c("0" = "#542788", "10000" = "#542788", "20000" = "#542788", "30000" = "#2d004b", "40000" = "#2d004b", "50000" = "#2d004b", "60000" = "#7f3b08", "70000" = "#7f3b08", "80000" = "#7f3b08", "90000" = "#b35806", "100000" = "#b35806", "110000" = "#b35806", "120000" = "#fdb863", "130000" = "#fdb863", "140000" = "#fdb863", "150000" = "#fee0b6", "160000" = "#fee0b6", "170000" = "#fee0b6", "180000" = "#d8daeb", "190000" = "#d8daeb", "200000" = "#d8daeb", "210000" = "#b2abd2", "220000" = "#b2abd2", "230000" = "#b2abd2")


cluster_results %>%
  filter(cluster_number == 1) %>% 
  ggplot(., aes(x = new_position, y = index_value)) +
  geom_line(aes(colour = as.factor(new_time), linetype = index_name)) +
  scale_color_manual(values = colours) +
  facet_wrap(.~point)
  
cluster_results %>%
  filter(cluster_number == 2) %>% 
  ggplot(., aes(x = new_position, y = index_value)) +
  geom_line(aes(colour = as.factor(new_time), linetype = index_name)) +
  scale_color_manual(values = colours) +
  facet_wrap(.~point)

cluster_results %>%
  filter(cluster_number == 3) %>% 
  ggplot(., aes(x = new_position, y = index_value)) +
  geom_line(aes(colour = as.factor(new_time), linetype = index_name)) +
  scale_color_manual(values = colours) +
  facet_wrap(.~point)

cluster_results %>%
  filter(cluster_number == 4) %>% 
  ggplot(., aes(x = new_position, y = index_value)) +
  geom_line(aes(colour = as.factor(new_time), linetype = index_name)) +
  scale_color_manual(values = colours) +
  facet_wrap(.~point)

cluster_results %>%
  filter(cluster_number == 5) %>% 
  ggplot(., aes(x = new_position, y = index_value)) +
  geom_line(aes(colour = as.factor(new_time), linetype = index_name)) +
  scale_color_manual(values = colours) +
  facet_wrap(.~point)

