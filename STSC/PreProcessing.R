library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SpectralIndices_Channel1", "SummaryIndices_Channel1_WindRemoved.csv"))



df_order <- df %>% 
  #filter(., year == "2015" & month == "3") %>% 
  with(., .[order(date, time, ResultMinute),]) %>% 
  select(., AcousticComplexity, EventsPerSecond, TemporalEntropy)

write.csv(df_order, getDataPath("STSC", "SERF", "201503_SERF.csv"))

df_index <- select(df_order, AcousticComplexity) %>% 
  write.table(., getDataPath("STSC", "201503_SERF_ACI.txt"), row.names = FALSE, col.names = FALSE)



                        
motifs <- read.table("C:/Work/STSC/resACI_432.txt")
with(motifs, motifs[order(V1),])

unique(df$ID)
