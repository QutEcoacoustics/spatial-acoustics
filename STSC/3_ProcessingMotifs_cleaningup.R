library(tidyverse)
library(ggplot2)

rm(list=ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

point <- "WBA2O"

data <- "Bowraaug"

#EventsPerSecond
index <- "EVN"
index_select <- "EventsPerSecond"

#List motif result files + time series files
filename_results <- paste("res", index, "_", point, "_432.txt", sep = "") 
filename_complete_ts <- paste(point, data, ".csv", sep = "")

complete_ts <- read.csv(getDataPath("Results", data, point,  filename_complete_ts)) %>% 
  rename(., position = X)


#Plotting TS for all indices
complete_ts %>% select(., 1:4) %>% 
  pivot_longer(., cols = 2:4, names_to = "index", values_to = "value") %>% 
  ggplot(., aes(x = position, y = value)) +
  geom_line() +
  facet_wrap(. ~ index) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  ggsave(getDataPath("Results", data, point, "indicespertime.jpg"))
 

#TS Graphs - Checking for overlaps and true positives


ts <- complete_ts %>%
  mutate(., point = point) %>% 
  select(., 1, all_of(index_select), 5:ncol(.)) %>% 
  mutate(., motif = NA) %>% 
  mutate(., distance = NA) %>% 
  mutate(., length = NA) %>% 
  mutate(., reference = "0_ts") %>% 
  mutate(., id = 0) %>% 
  rename(., Index = index_select)

res <- read.table(getDataPath("Results", data, point, filename_results))

res <- rename(res, FirstInstance_Start = V1,
                                  FirstInstance_End = V2,
                                  SecondInstance_Start = V3,
                                  SecondInstance_End = V4,
                                  Length = V5,
                                  Distance = V6) %>%
  mutate(., id = 1:as.numeric(count(res))) %>% 
  filter(., Distance <= 5) %>% 
  select(., id, everything()) %>% 
  pivot_longer(., cols = 2:5, names_to = "Instance", values_to = "position") %>% 
  mutate(., Instance = gsub(pattern = "FirstInstance", replacement = "motif", x = Instance)) %>%
  mutate(., Instance = gsub(pattern = "SecondInstance", replacement = "match", x = Instance)) %>%
  separate(., Instance, into = c("instance", "moment"), sep = "_") %>% 
  pivot_wider(., names_from = moment, values_from = position)


#Motif results                         
  
res_motif <- mutate(res, instance = paste(id, instance, sep = "_")) %>% 
                          with(., .[order(Start),]) %>% 
                          mutate(., overlap = NA)

for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] <= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] - res_motif$End[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$Start[row+1] - res_motif$Start[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 30 & res_motif$Start[row+1] - res_motif$End[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     TRUE ~ as.character(res_motif$Distance[row]))
  
}


res_motif <- filter(res_motif, overlap != "repeated") %>%
  with(., .[order(Start),]) 


for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Distance[row] <= res_motif$overlap[row] ~ "keep",
                                     TRUE ~ "repeated")
  
}

res_motif <- filter(res_motif, overlap == "keep") %>% 
  with(., .[order(Start),])



for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] <= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] - res_motif$End[row] <= 30 ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$Start[row+1] - res_motif$Start[row] <= 30 ~ "repeated",
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 30 & res_motif$Start[row+1] - res_motif$End[row] <= 30 ~ "repeated",
                                     TRUE ~ res_motif$instance[row])
  
} 




res_motif <- filter(res_motif, overlap != "repeated")


for (row in 1:nrow(ts)) {
  
ts[res_motif$Start[row]:res_motif$End[row], c("motif", "distance", "length")] <- res_motif[row, c("instance", "Distance", "Length")]
}

write.csv(ts, getDataPath("Results", data, point, paste(point, index, "motif.csv", sep = "_")))

#Preparing for the plot
plot_ts <- select(ts, reference, position, Index)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA")
  
plot_df_motif <- rbind(plot_ts, plot_motif) %>% 
  separate(., reference, into = c("number", "what"), remove = F)


plot_ts <- select(ts, reference, position, Index) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

ggplot(plot_ts, aes(x = position, y = Index)) +
  geom_line(aes(colour = what, linetype = what), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = Index, colour = "red")) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  theme_classic() +
  labs(title = paste(index, point, sep = " ")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  ggsave(getDataPath("Results", data, point, paste(point, index, "ts_motifs.jpg", sep = "")))

#TemporalEntropy

index <- "ENT"
index_select <- "TemporalEntropy"

filename_results <- paste("res", index, "_", point, "_432.txt", sep = "") 
filename_complete_ts <- paste(point, data, ".csv", sep = "")

complete_ts <- read.csv(getDataPath("Results", data, point, filename_complete_ts)) %>% 
  rename(., position = X)


#Plotting TS for all indices
complete_ts %>% select(., 1:4) %>% 
  pivot_longer(., cols = 2:4, names_to = "index", values_to = "value") %>% 
  ggplot(., aes(x = position, y = value)) +
  geom_line() +
  facet_wrap(. ~ index) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  ggsave(getDataPath("Results", data, point, "indicespertime.jpg"))


ts <- complete_ts %>%
  mutate(., point = point) %>% 
  select(., 1, all_of(index_select), 5:ncol(.)) %>% 
  mutate(., motif = NA) %>% 
  mutate(., distance = NA) %>% 
  mutate(., length = NA) %>% 
  mutate(., reference = "0_ts") %>% 
  mutate(., id = 0) %>% 
  rename(., Index = index_select)

res <- read.table(getDataPath("Results", data, point, filename_results))

res <- rename(res, FirstInstance_Start = V1,
              FirstInstance_End = V2,
              SecondInstance_Start = V3,
              SecondInstance_End = V4,
              Length = V5,
              Distance = V6) %>%
  mutate(., id = 1:as.numeric(count(res))) %>% 
  filter(., Distance <= 5) %>% 
  select(., id, everything()) %>% 
  pivot_longer(., cols = 2:5, names_to = "Instance", values_to = "position") %>% 
  mutate(., Instance = gsub(pattern = "FirstInstance", replacement = "motif", x = Instance)) %>%
  mutate(., Instance = gsub(pattern = "SecondInstance", replacement = "match", x = Instance)) %>%
  separate(., Instance, into = c("instance", "moment"), sep = "_") %>% 
  pivot_wider(., names_from = moment, values_from = position)


#Motif results                         

res_motif <- mutate(res, instance = paste(id, instance, sep = "_")) %>% 
  with(., .[order(Start),]) %>% 
  mutate(., overlap = NA)

for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] <= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] - res_motif$End[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$Start[row+1] - res_motif$Start[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 30 & res_motif$Start[row+1] - res_motif$End[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     TRUE ~ as.character(res_motif$Distance[row]))
  
}


res_motif <- filter(res_motif, overlap != "repeated") %>%
  with(., .[order(Start),]) 


for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Distance[row] <= res_motif$overlap[row] ~ "keep",
                                     TRUE ~ "repeated")
  
}

res_motif <- filter(res_motif, overlap == "keep") %>% 
  with(., .[order(Start),])



for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] <= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] - res_motif$End[row] <= 30 ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$Start[row+1] - res_motif$Start[row] <= 30 ~ "repeated",
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 30 & res_motif$Start[row+1] - res_motif$End[row] <= 30 ~ "repeated",
                                     TRUE ~ res_motif$instance[row])
  
} 




res_motif <- filter(res_motif, overlap != "repeated")


for (row in 1:nrow(ts)) {
  
  ts[res_motif$Start[row]:res_motif$End[row], c("motif", "distance", "length")] <- res_motif[row, c("instance", "Distance", "Length")]
}

write.csv(ts, getDataPath("Results", data, point, paste(point, index, "motif.csv", sep = "_")))

#Preparing for the plot
plot_ts <- select(ts, reference, position, Index)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA")

plot_df_motif <- rbind(plot_ts, plot_motif) %>% 
  separate(., reference, into = c("number", "what"), remove = F)


plot_ts <- select(ts, reference, position, Index) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

ggplot(plot_ts, aes(x = position, y = Index)) +
  geom_line(aes(colour = what, linetype = what), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = Index, colour = "red")) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  theme_classic() +
  labs(title = paste(index, point, sep = " ")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  ggsave(getDataPath("Results", data, point, paste(point, index, "ts_motifs.jpg", sep = "")))

#AcousticComplexity
index <- "ACI"
index_select <- "AcousticComplexity"


filename_results <- paste("res", index, "_", point, "_432.txt", sep = "") 
filename_complete_ts <- paste(point, data, ".csv", sep = "")

complete_ts <- read.csv(getDataPath("Results", data, point, filename_complete_ts)) %>% 
  rename(., position = X)


#Plotting TS for all indices
complete_ts %>% select(., 1:4) %>% 
  pivot_longer(., cols = 2:4, names_to = "index", values_to = "value") %>% 
  ggplot(., aes(x = position, y = value)) +
  geom_line() +
  facet_wrap(. ~ index) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  ggsave(getDataPath("Results", data, point, "indicespertime.jpg"))


ts <- complete_ts %>%
  mutate(., point = point) %>% 
  select(., 1, all_of(index_select), 5:ncol(.)) %>% 
  mutate(., motif = NA) %>% 
  mutate(., distance = NA) %>% 
  mutate(., length = NA) %>% 
  mutate(., reference = "0_ts") %>% 
  mutate(., id = 0) %>% 
  rename(., Index = index_select)

res <- read.table(getDataPath("Results", data, point, filename_results))

res <- rename(res, FirstInstance_Start = V1,
              FirstInstance_End = V2,
              SecondInstance_Start = V3,
              SecondInstance_End = V4,
              Length = V5,
              Distance = V6) %>%
  mutate(., id = 1:as.numeric(count(res))) %>% 
  filter(., Distance <= 5) %>% 
  select(., id, everything()) %>% 
  pivot_longer(., cols = 2:5, names_to = "Instance", values_to = "position") %>% 
  mutate(., Instance = gsub(pattern = "FirstInstance", replacement = "motif", x = Instance)) %>%
  mutate(., Instance = gsub(pattern = "SecondInstance", replacement = "match", x = Instance)) %>%
  separate(., Instance, into = c("instance", "moment"), sep = "_") %>% 
  pivot_wider(., names_from = moment, values_from = position)


#Motif results                         

res_motif <- mutate(res, instance = paste(id, instance, sep = "_")) %>% 
  with(., .[order(Start),]) %>% 
  mutate(., overlap = NA)

for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] <= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] - res_motif$End[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$Start[row+1] - res_motif$Start[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 30 & res_motif$Start[row+1] - res_motif$End[row] <= 30 ~ as.character(res_motif$Distance[row+1]),
                                     TRUE ~ as.character(res_motif$Distance[row]))
  
}


res_motif <- filter(res_motif, overlap != "repeated") %>%
  with(., .[order(Start),]) 


for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Distance[row] <= res_motif$overlap[row] ~ "keep",
                                     TRUE ~ "repeated")
  
}

res_motif <- filter(res_motif, overlap == "keep") %>% 
  with(., .[order(Start),])



for (row in 1:nrow(res_motif)) {
  
  res_motif$overlap[row] = case_when(res_motif$Start[row+1] <= res_motif$Start[row] & res_motif$End[row+1] >= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] <= res_motif$End[row] ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$End[row+1] - res_motif$End[row] <= 30 ~ "repeated",
                                     res_motif$Start[row+1] <= res_motif$End[row] & res_motif$Start[row+1] - res_motif$Start[row] <= 30 ~ "repeated",
                                     res_motif$Start[row+1] - res_motif$Start[row] <= 30 & res_motif$Start[row+1] - res_motif$End[row] <= 30 ~ "repeated",
                                     TRUE ~ res_motif$instance[row])
  
} 




res_motif <- filter(res_motif, overlap != "repeated")





for (row in 1:nrow(ts)) {
  
  ts[res_motif$Start[row]:res_motif$End[row], c("motif", "distance", "length")] <- res_motif[row, c("instance", "Distance", "Length")]
}

write.csv(ts, getDataPath("Results", data, point, paste(point, index, "motif.csv", sep = "_")))

#Preparing for the plot
plot_ts <- select(ts, reference, position, Index)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA")

plot_df_motif <- rbind(plot_ts, plot_motif) %>% 
  separate(., reference, into = c("number", "what"), remove = F)


plot_ts <- select(ts, reference, position, Index) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_motif <- select(ts, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

ggplot(plot_ts, aes(x = position, y = Index)) +
  geom_line(aes(colour = what, linetype = what), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = Index, colour = "red")) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  theme_classic() +
  labs(title = paste(index, point, sep = " ")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  ggsave(getDataPath("Results", data, point, paste(point, index, "ts_motifs.jpg", sep = "")))

