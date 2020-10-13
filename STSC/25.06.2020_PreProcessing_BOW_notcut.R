library(tidyverse)
library(BNPTSclust)
library(ggplot2)


rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

point <- "WB56"
#batch <- "201503_"
filename_complete_ts <- paste(point, ".csv", sep = "")

complete_ts <- read.csv(getDataPath("Results", point, filename_complete_ts)) %>% 
  rename(., position = X)


parameters <- read.csv(getDataPath("Results", point, "motif_info2.csv"))

obs <- 1

index <- parameters$index_abb[obs]
index_select <- parameters$index_name[obs]
#cut_threshold <- parameters$threshold[obs]
filename_results <- parameters$filename[obs]  

#TS Graphs - Checking for overlaps and true positives


ts <- complete_ts %>%
  mutate(., point = point) %>% 
  dplyr::select(., 1, all_of(index_select), 5:13) %>% 
  mutate(., motif = NA) %>% 
  mutate(., distance = NA) %>% 
  mutate(., length = NA) %>% 
  mutate(., reference = "0_ts") %>% 
  mutate(., id = 0) %>% 
  rename(., Index = index_select)

res <- read.table(getDataPath("Results", point, filename_results))

res <- rename(res, FirstInstance_Start = V1,
              FirstInstance_End = V2,
              SecondInstance_Start = V3,
              SecondInstance_End = V4,
              Length = V5,
              Distance = V6) %>%
  mutate(., id = 1:as.numeric(count(res))) %>% 
  #filter(., Distance <= cut_threshold) %>% 
  dplyr::select(., id, everything()) %>% 
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

write.csv(ts, getDataPath("Results", point, paste(point, index, "motif_09102020.csv", sep = "_")))

#Preparing for the plot
plot_ts <- select(ts, reference, position, Index)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA")

plot_df_motif <- rbind(plot_ts, plot_motif) %>% 
  separate(., reference, into = c("number", "what"), remove = F)


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

write.csv(ts, getDataPath("Results", point, paste(point, index, "match_09102020.csv", sep = "_")))
