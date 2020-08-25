library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Chapter2_SoundscapeTemporalAssessment", "SERF_AI_PreProcessed", "21.07.2020_151617FinalIndices.csv"))

df_order <- df %>% 
  #filter(., ID == "WB56") %>% 
  with(., .[order(date, time, ResultMinute),]) %>% 
  select(., AcousticComplexity, BackgroundNoise, HighFreqCover, LowFreqCover, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfCoVSpectrum, EntropyOfPeaksSpectrum, ClusterCount, Ndsi, FileName, date, ResultMinute) %>% 
  write.csv(., getDataPath("STSC", "SERF", "SERF.csv"))

df_index <- select(df_order, Ndsi) %>% 
  write.table(., getDataPath("STSC", "SERF", "SERF_NDSI.txt"), row.names = FALSE, col.names = FALSE)
                        
motifs <- read.table("C:/Work/STSC/resACI_432.txt")
with(motifs, motifs[order(V1),])

unique(df$ID)
