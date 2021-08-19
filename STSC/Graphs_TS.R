library(tidyverse)
library(caret)
library(repr)
library(data.table)
library(TTR)
library(forecast)
library(lubridate)
library(randomForest)
library(wavelets)
library(party)
library(vegan)
library(dtwclust)
library(purrr)

rm(list = ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

data <- "Bowraaug"
point_name <- "WB047"

#EventsPerSecond
 index_name <- "EventsPerSecond"
 index_abb <- "EVN"

#List motif result files + time series files

complete_ts <- read.csv(getDataPath("STSC", "Results", data, point_name, paste(point_name, data, ".csv", sep = ""))) %>%
  rename(., position = X)


# df with the motifs and filenames

motifs <- read.csv(getDataPath("STSC", "Results", data, point_name, paste(data, "motif_complete.csv", sep = "")))


ts_data <- select(motifs, index_value, position, id, distance, length) %>%
  group_by(., id) %>%
  mutate(., new_position = order(order(position))) %>%
  ungroup(.) %>%
  select(., everything(), -position) %>%
  pivot_wider(., names_from = new_position, values_from = index_value)

summary(ts_data)
sd(ts_data$distance)
sd(ts_data$length)

plot_ts <- select(complete_ts, all_of(index_name), position, date, time, ResultMinute) %>% 
  # filter(day == "10") %>% 
  droplevels(.)

plot_motif <- select(motifs, id, position, index_value, date) %>% 
  rename(., reference = id) %>% 
  #filter(reference != "NA") %>% 
  separate(., reference, into = c("point", "index", "number", "what"), remove = F) %>% 
  filter(., index == index_abb) %>% 
  # filter(., date == "20150310") %>% 
  droplevels(.)



ggplot(plot_ts, aes(x = position, y = EventsPerSecond)) +
  geom_line(aes(colour = index_value), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = index_value, colour = reference)) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  #theme_classic() +
  #facet_wrap(.~point) +
  labs(title = paste(index_name, "Bowra", sep = " ")) +
  scale_color_manual(values = c("#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f", "#2ca25f")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_blank(), plot.background = element_rect(fill = NA), panel.background = element_rect(fill = NA)) +
  geom_vline(xintercept = c(420, 1862), linetype = "dotted") #+
  ggsave(getDataPath("Chapter1_FineScaleAcousticSurvey", "Figures", paste("test_", data, index_abb, point_name, "ts_motifs.png", sep = "")),  width = 4.800, height = 0.987, dpi = 300)

#AcousticComplexity
index_name <- "AcousticComplexity"
index_abb <- "ACI"

#List motif result files + time series files

# complete_ts <- read.csv(getDataPath("STSC", "Results", data, paste(data, "_complete_ts.csv", sep = ""))) #%>% 
# rename(., position = X)
# 
# 
# # df with the motifs and filenames
# 
# motifs <- read.csv(getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = "")))


ts_data <- select(motifs, index_value, position, id, distance, length) %>%
  group_by(., id) %>%
  mutate(., new_position = order(order(position))) %>%
  ungroup(.) %>%
  select(., everything(), -position) %>%
  pivot_wider(., names_from = new_position, values_from = index_value)

summary(ts_data)
sd(ts_data$distance)
sd(ts_data$length)

plot_ts <- select(complete_ts, all_of(index_name), position, date, time, ResultMinute) %>% 
  # filter(day == "10") %>% 
  droplevels(.)

plot_motif <- select(motifs, id, position, index_value, date) %>% 
  rename(., reference = id) %>% 
  #filter(reference != "NA") %>% 
  separate(., reference, into = c("point", "index", "number", "what"), remove = F) %>% 
  filter(., index == index_abb) %>% 
  filter(., date == "20150310") %>% 
  droplevels(.)


ggplot(plot_ts, aes(x = position, y = AcousticComplexity)) +
  geom_line(aes(colour = index_value), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = index_value, colour = reference)) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  #theme_classic() +
  #facet_wrap(.~point) +
  #labs(title = paste(index_name, "Bowra", sep = " ")) +
  scale_color_manual(values = c("#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7", "#8856a7")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_blank(), plot.background = element_rect(fill = NA), panel.background = element_rect(fill = NA)) +
  geom_vline(xintercept = c(420, 1862), linetype = "dotted") #+
  ggsave(getDataPath("STSC", "Methods_Paper", "Figures", paste(data, index_abb, point_name, "ts_motifs.png", sep = "")), width = 4.800, height = 0.987, dpi = 300)

#TemporalEntropy
index_name <- "TemporalEntropy"
index_abb <- "ENT"

#List motif result files + time series files

# complete_ts <- read.csv(getDataPath("STSC", "Results", data, paste(data, "_complete_ts.csv", sep = ""))) #%>% 
# rename(., position = X)


# df with the motifs and filenames

# motifs <- read.csv(getDataPath("STSC", "Results", data, paste(data, "motif_complete.csv", sep = "")))


ts_data <- select(motifs, index_value, position, id, distance, length) %>%
  group_by(., id) %>%
  mutate(., new_position = order(order(position))) %>%
  ungroup(.) %>%
  select(., everything(), -position) %>%
  pivot_wider(., names_from = new_position, values_from = index_value)

summary(ts_data)
sd(ts_data$distance)
sd(ts_data$length)

plot_ts <- select(complete_ts, all_of(index_name), position, date, time, ResultMinute) %>% 
  #filter(day == "10") %>% 
  droplevels(.)

plot_motif <- select(motifs, id, position, index_value, date) %>% 
  rename(., reference = id) %>% 
  #filter(reference != "NA") %>% 
  separate(., reference, into = c("point", "index", "number", "what"), remove = F) %>% 
  filter(., index == index_abb) %>% 
  #filter(., date == "20150310") %>% 
  droplevels(.)


ggplot(plot_ts, aes(x = position, y = index_value)) +
  geom_line(aes(colour = index_value), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = index_value, colour = reference)) +
  #geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  #theme_classic() +
  #facet_wrap(.~point) +
  #labs(title = paste(index_name, "Bowra", sep = " ")) +
  scale_color_manual(values = c("#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca", "#43a2ca")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_blank(), plot.background = element_rect(fill = NA), panel.background = element_rect(fill = NA)) +
  geom_vline(xintercept = c(420, 1862), linetype = "dotted") #+
  ggsave(getDataPath("STSC", "Methods_Paper", "Figures", paste(data, index_abb, point_name, "ts_motifs.png", sep = "")), width = 4.800, height = 0.987, dpi = 300)


#1440, 2880, 4320, 5760, 7200, 8640, 10080, 11520, 12960, 14400, 15840, 17280, 18720, 20160, 21600, 23040, 24480, 25920, 27360, 28800, 30240, 31680

rownames(ts_data) <- ts_data$id
ts_data <- ts_data[,2:length(ts_data)]


ts_list <- transpose(ts_data) %>% 
  map(., na.omit) 


wtData <- NULL


for (i in ts_list) {
  
  wt <- dwt(i, filter="haar", boundary= "periodic")
  
  un <- as.data.frame(t(unlist(c(wt@W,wt@V[[wt@level]]))))
  
  wtData <- plyr::rbind.fill(wtData, un)
  
}

rownames(wtData) <- rownames(ts_data)

write.csv(wtData, getDataPath("Chapter1_FineScaleAcousticSurvey", "DiscriminantAnalysis", paste(data, "wavelet_0.csv", sep = "")), row.names = T)
