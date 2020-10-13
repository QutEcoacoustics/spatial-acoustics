library(tidyverse)
library(ggplot2)

rm(list=ls())

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/Results",  ...))
}

point <- "SERF"
batch <- 20133
index <- "ACI"

df_motif <- read.csv(getDataPath(point, paste(point, index, batch, "motif.csv", sep = "_")))
df_match <- read.csv(getDataPath(point, paste(point, index, batch, "match.csv", sep = "_")))

ts <- filter(df_motif, reference == "0_ts")

motif <- filter(df_motif, motif != "NA")

match <- filter(df_match, match != "NA")


#Preparing for the plot
plot_ts <- select(ts, reference, position, Index) %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_motif <- select(motif, motif, position, Index) %>% 
  rename(., reference = motif) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

plot_match <- select(match, match, position, Index) %>% 
  rename(reference = match) %>% 
  filter(reference != "NA") %>% 
  separate(., reference, into = c("number", "what"), remove = F)

ggplot(plot_ts, aes(x = position, y = Index)) +
  geom_line(aes(colour = what, linetype = what), colour = "grey") +
  geom_line(data = plot_motif, aes(x = position, y = Index, colour = number)) +
  geom_line(data = plot_match, aes(x = position, y = Index, colour = number)) + 
  scale_linetype_manual(values = "dotted") +
  theme_classic() +
  labs(title = paste(index, point, sep = " ")) +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
ggsave(getDataPath(point, "Figures", paste(index, "ts_motifs_matches.jpg"), sep = ""))
