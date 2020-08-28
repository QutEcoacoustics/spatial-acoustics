rm(list=ls())

library(tidyverse)
library(ggplot2)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}


point <- "WB06"
filename_complete_ts <- "WB06_OCT.csv"
index <- "BGN"
cut_threshold <- "2.289616"
filename_results <- "resBGN_wb06_432.txt"



#TS Graphs - Checking for overlaps and true positives

complete_ts <- read.csv(getDataPath(Point, filename_complete_ts)) %>% 
  rename(., position = X)
  

#Plotting TS for all indices
complete_ts %>% select(., 1:8) %>% 
  pivot_longer(., cols = 2:8, names_to = "index", values_to = "value") %>% 
  ggplot(., aes(x = position, y = value)) +
  geom_line() +
  facet_wrap(. ~ index) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  ggsave(getDataPath(point, "28.08.2020_indicespertime.jpg"))


ts <- complete_ts %>% 
  mutate(., point = point) %>% 
  select(., 1:2, 9:18) %>% 
  mutate(., motif = NA) %>% 
  mutate(., distance = NA) %>% 
  mutate(., length = NA) %>% 
  mutate(., reference = "ts") %>% 
  mutate(., id = 0)

res <- read.table(getDataPath(point, index, filename_results)) %>%
                          rename(., FirstInstance_Start = V1,
                                  FirstInstance_End = V2,
                                  SecondInstance_Start = V3,
                                  SecondInstance_End = V4,
                                  Length = V5,
                                  Distance = V6) %>%
                          mutate(., id = 1:as.numeric(count(res))) %>%
                          filter(., Distance <= cut_threshold) %>% 
                          select(., id, everything()) %>% 
                          pivot_longer(., cols = 2:5, names_to = "Instance", values_to = "position") %>% 
                          mutate(., Instance = gsub(pattern = "FirstInstance", replacement = "motif", x = Instance)) %>%
                          mutate(., Instance = gsub(pattern = "SecondInstance", replacement = "match", x = Instance)) %>%
                          separate(., Instance, into = c("instance", "moment"), sep = "_") %>% 
                          pivot_wider(., names_from = moment, values_from = position)
#Motif results                         
  
res_motif <- filter(res, instance == "motif") %>% 
                          mutate(., instance = paste(id, instance, sep = "_")) %>% 
                          with(., .[order(Start),]) %>% 
                          mutate(., overlap = NA)

for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 3 & res_motif$Start[row+1] - res_motif$End[row] <= 3 ~ "repeated",
                                TRUE ~ "motif")
  
} 

res_motif <- filter(res_motif, overlap != "repeated")


for (row in 1:nrow(ts)) {
  
ts[res_motif$Start[row]:res_motif$End[row], c("motif", "distance", "length")] <- res_motif[row, c("instance", "Distance", "Length")]
}

#Preparing for the plot
plot_ts <- select(ts, reference, position, AcousticComplexity, id)
plot_motif <- select(ts, motif, position, AcousticComplexity, id) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA")
plot_df <- rbind(plot_ts, plot_motif)

rm(plot_ts, plot_motif)

colours <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#35978f", "#1a1a1a")
linetype <- c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dotted")

ggplot(plot_df, aes(x = position, y = AcousticComplexity)) +
  geom_line(aes(colour = reference, linetype = reference)) +
  scale_colour_manual(values =  colours) +
  scale_linetype_manual(values = linetype) +
  theme_classic() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(getDataPath(point, index, "Figures", "28.08.2020_Motifs.jpg"))


#Match results

res_match <- filter(res, instance == "match") %>% 
  mutate(., instance = paste(id, instance, sep = "_")) %>% 
  with(., .[order(Start),]) %>% 
  mutate(., overlap = NA)

for (row in 1:nrow(res_match)) {
  
  res_match$overlap[row] = case_when(res_match$Start[row+1] <= res_match$Start[row] & res_match$End[row+1] >= res_match$End[row] ~ "repeated",
                                     res_match$Start[row+1] - res_match$Start[row] <= 3 & res_match$Start[row+1] - res_match$End[row] <= 3 ~ "repeated",
                                     TRUE ~ "match")
  
} 

res_match <- filter(res_match, overlap != "repeated")


for (row in 1:nrow(ts)) {
  
  ts[res_match$Start[row]:res_match$End[row], c("match", "distance", "length")] <- res_match[row, c("instance", "Distance", "Length")]
}

#Preparing for the plot
plot_ts <- select(ts, reference, position, AcousticComplexity, id)
plot_match <- select(ts, match, position, AcousticComplexity, id) %>% 
  rename(., reference = match) %>% 
  filter(reference != "NA")
plot_df <- rbind(plot_ts, plot_match)

rm(plot_ts, plot_match)

#"#a6cee3", "#1f78b4"

colours <- c("#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#35978f", "#1a1a1a")
linetype <- c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dotted")

ggplot(plot_df, aes(x = position, y = AcousticComplexity)) +
  geom_line(aes(colour = reference, linetype = reference)) +
  scale_colour_manual(values =  colours) +
  scale_linetype_manual(values = linetype) +
  theme_classic() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(getDataPath(point, index, "Figures", "28.08.2020_matches.jpg"))



