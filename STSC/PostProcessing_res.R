library(tidyverse)
library(ggplot2)

rm(list=ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

point <- "SERF"
batch <- "201503_"
filename_complete_ts <- paste(batch, point, ".csv", sep = "")

complete_ts <- read.csv(getDataPath(point, filename_complete_ts)) %>% 
  rename(., position = X)


#Plotting TS for all indices
complete_ts %>% select(., 1:8) %>% 
  pivot_longer(., cols = 2:8, names_to = "index", values_to = "value") %>% 
  ggplot(., aes(x = position, y = value)) +
  geom_line() +
  facet_wrap(. ~ index) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  ggsave(getDataPath(point, "04.09.2020_indicespertime.jpg"))


parameters <- read.csv(getDataPath(point, "motif_info.csv"))

obs <- 5

index <- "ACI"
index_select <- "AcousticComplexity"
cut_threshold <- 2.698956
filename_results <- "resACI_SERF_430.txt"  

#TS Graphs - Checking for overlaps and true positives


ts <- complete_ts %>%
  mutate(., point = point) %>% 
  select(., 1, all_of(index_select), 12:14) %>% 
  mutate(., motif = NA) %>% 
  mutate(., distance = NA) %>% 
  mutate(., length = NA) %>% 
  mutate(., reference = "0_ts") %>% 
  mutate(., id = 0) %>% 
  rename(., Index = index_select)

res <- read.table(getDataPath("resACI_SERF_430.txt"))

res <- rename(res, FirstInstance_Start = V1,
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

write.csv(ts, getDataPath(point, paste(point, index, "motif.csv", sep = "_")))

#Preparing for the plot
plot_ts <- select(ts, reference, position, Index)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA")
  
plot_df_motif <- rbind(plot_ts, plot_motif) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

# , 

colours <- c("#1a1a1a", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#35978f", "#ff7f00", "#cab2d6", "#6a3d9a")
linetype <- c("solid", "dotted")

ggplot(plot_df_motif, aes(x = position, y = Index)) +
  geom_line(aes(colour = number, linetype = what)) +
  #scale_colour_manual(values =  colours) +
  scale_linetype_manual(values = linetype) +
  theme_classic() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  labs(y = index_select) +
  ggsave(getDataPath(point, paste(index, "Motifs.jpg"), sep = ""))

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

write.csv(ts, getDataPath(point, paste(point, index, "match.csv", sep = "_")))

#Preparing for the plot
plot_match <- select(ts, match, position, Index) %>% 
  rename(., reference = match) %>% 
  filter(., reference != "NA")
  
plot_df_match <- rbind(plot_ts, plot_match) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

rm(plot_ts)


#"#a6cee3", "#1f78b4" "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",

ggplot(plot_df_match, aes(x = position, y = Index)) +
  geom_line(aes(colour = number, linetype = what)) +
  #scale_colour_manual(values =  colours) +
  scale_linetype_manual(values = linetype) +
  theme_classic() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  labs(y = index_select) +
  ggsave(getDataPath(point, paste(index, "matches.jpg"), sep = ""))


#Plotting the motifs and matches
match_motif_df <- rbind(plot_df_match, plot_df_motif) %>% 
  filter(., reference != "0_ts") %>% 
  group_by(., number, what) %>% 
  mutate(., id = order(order(position))) %>% 
  ungroup(.)


ggplot(match_motif_df, aes(x = id, y = Index)) +
  geom_line(aes(linetype = what)) +
  scale_colour_manual(values =  colours) +
  scale_linetype_manual(values = linetype) +
  theme_classic() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  labs(y = index_select) +
  facet_wrap(. ~ number, nrow = 4, ncol = 5) +
  ggsave(getDataPath(point, paste(index, "04.09.2020_matches_motifs.jpg")))
