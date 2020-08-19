library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Chapter1_FineScaleAcousticSurvey", "23.07.2020_scaledvars_complete.csv"))

df_order <- df %>% 
  with(., .[order(ID, Date, beginning_rec, ResultMinute),]) %>% 
  select(., AcousticComplexity, BackgroundNoise, HighFreqCover, LowFreqCover, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, X.1.x, dominant_sound, second_dominant, obs, FileName, Date, beginning_rec, ResultMinute, time_rec) %>% 
  write.csv("C:/Work/STSC/18.08.2020_OCT.csv")
                        
motifs <- read.table("C:/Work/STSC/resACI_432.txt")
with(motifs, motifs[order(V1),])

unique(df$ID)
