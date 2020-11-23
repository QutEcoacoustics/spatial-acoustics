library(tidyverse)
library(ggplot2)

rm(list=ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC",  ...))
}

complete_ts <- list.files(getDataPath("Results", "Bow_aug"), pattern = "Bowraaug.csv")


# for (file in complete_ts) {
#     read.csv(getDataPath("Results", "Bow_aug", file)) %>% 
#     rename(., position = X) %>% 
#     select(., 1:4) %>% 
#     pivot_longer(., cols = 2:4, names_to = "index", values_to = "value") %>% 
#     ggplot(., aes(x = position, y = value)) +
#     geom_line() +
#     facet_wrap(. ~ index) +
#     theme_classic() +
#     theme(axis.text.x = element_blank()) +
#     ggsave(getDataPath("Results", "Figures", paste(file, "indicespertime.jpg", sep = "_")))
#   
# }


#Analysis per index

index_name <- "EventsPerSecond"
index_abb <- "EVN"

result_files <- list.files(getDataPath("Results", "Bow_aug"), pattern = paste("res", index_abb, sep = ""))


res_motif_all <- data.frame(id = integer(),
                      Lenght = integer(),
                      Distance = numeric(),
                      instance = character(),
                      Start = integer(),
                      End = integer(),
                      FileName = character(),
                      overlap = logical())


for (file in result_files) {
  res_motif <- read.table(getDataPath("Results", "Bow_aug", file)) %>% 
    rename(., FirstInstance_Start = V1,
           FirstInstance_End = V2,
           SecondInstance_Start = V3,
           SecondInstance_End = V4,
           Length = V5,
           Distance = V6)
  res_motif <-  mutate(res_motif, id = 1:as.numeric(count(res_motif))) %>% 
    select(., id, everything()) %>% 
    pivot_longer(., cols = 2:5, names_to = "Instance", values_to = "position") %>% 
    mutate(., Instance = gsub(pattern = "FirstInstance", replacement = "motif", x = Instance)) %>%
    mutate(., Instance = gsub(pattern = "SecondInstance", replacement = "match", x = Instance)) %>%
    separate(., Instance, into = c("instance", "moment"), sep = "_") %>% 
    pivot_wider(., names_from = moment, values_from = position) %>% 
    mutate(., FileName = file) %>% 
    mutate(., instance = paste(id, instance, sep = "_")) %>% 
    with(., .[order(Start),]) %>% 
    mutate(., overlap = NA)
  res_motif_all <- rbind(res_motif, res_motif_all)
  
}
  
for (row in 1:nrow(res_motif_all)) {
  
  res_motif_all$overlap[row] = case_when(res_motif_all$Start[row+1] <= res_motif_all$Start[row] & res_motif_all$End[row+1] >= res_motif_all$End[row] ~ "repeated",
                                         res_motif_all$Start[row+1] - res_motif_all$Start[row] <= 3 & res_motif_all$Start[row+1] - res_motif_all$End[row] <= 3 ~ "repeated",
                                         TRUE ~ res_motif_all$instance[row])
  
} 
  
res_motif <- filter(res_motif_all, overlap != "repeated")


#Selecting ACI and creating the TS

complete_ts_all <- data.frame(position = integer(),
                              Index = numeric(),
                              FileName = factor(),
                              originalfile = factor(),
                              Location = factor(),
                              point = factor(),
                              rec = factor(),
                              date = integer(),
                              time = integer(), 
                              ResultStartSeconds = integer(),
                              ResultStartSeconds = integer(),
                              motif = logical(), 
                              distance = logical(),
                              length = logical(),
                              reference = character(),
                              id = numeric())

for (file in complete_ts) {
  complete_ts <- read.csv(getDataPath("Results", "Bow_aug", file)) %>% 
    rename(., point = Transectpoint) %>% 
    select(., 1, all_of(index_name), 5:13) %>%
    rename(., Index = index_name) %>% 
    mutate(., motif = NA) %>% 
    mutate(., distance = NA) %>% 
    mutate(., length = NA) %>% 
    mutate(., reference = "0_ts") %>% 
    mutate(., id = 0)  %>% 
    mutate(., ts_id = paste(index_abb, gsub(x = file, pattern = ".csv", replacement = ""), sep = "_")) %>% 
    rename(., position = X)
   complete_ts_all <- rbind(complete_ts, complete_ts_all)
}


for (row in 1:nrow(complete_ts_all)) {
  
  complete_ts_all[res_motif$Start[row]:res_motif$End[row], c("motif", "distance", "length")] <- res_motif[row, c("instance", "Distance", "Length")]
}


for (id in unique(complete_ts_all$ts_id)) {
  write.csv(complete_ts_all, getDataPath("Results", "Bow_aug", paste(id, "motif.csv", sep = "_")), row.names = F)
  
}
  
plot_ts <- select(complete_ts_all, reference, position, Index, point) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_motif <- select(complete_ts_all, motif, position, Index, point) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_match <- select(complete_ts_all, motif, position, Index, point) %>% 
  rename(reference = motif) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

ggplot(plot_motif, aes(x = position, y = Index)) +
  geom_line(aes(colour = what, linetype = what), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = Index, colour = number)) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  theme_classic() +
  #labs(title = paste(index, point, sep = " ")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") #+
  #ggsave(getDataPath("Results", point, index, "Figures", paste(index, "ts_motifs_matches_20200922.jpg"), sep = ""))


