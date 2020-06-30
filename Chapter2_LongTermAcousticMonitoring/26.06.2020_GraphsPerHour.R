#Plotting indices per hour#
#Marina Scarpelli#

library(tidyverse)
library(ggplot2)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment",  ...))
}

#Reads the data
df <- read.csv(getDataPath("SERF_AI_PreProcessed", "26.06.2020_131415FinalIndices.csv"))

#Putting the indices as a variable
df_wider <- gather(df, c(BackgroundNoise, AvgSnrOfActiveFrames, EventsPerSecond, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfCoVSpectrum, ClusterCount, Ndsi, SptDensity), key = "Index", value = "value")

unique(df_wider$Index)

unique(df_wider$month)

summary(df_wider)

#Plotting all the sites together
df_wider %>% filter(Index == "AcousticComplexity") %>% 
ggplot(., aes(x = day, y = value, colour = as.factor(year))) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(name = "Index Scaled Values") +
  ggtitle("Scaled Indices Values by time of the day (in hours)") +
  facet_wrap(~ month) +
  scale_x_continuous(name = "Time (Beginning of recording in hours)", labels =  c("12:00 am", "02:00 am", "04:00 am", "06:00 am", "08:00 am", "10:00 am", "12:00 pm", "02:00 pm", "04:00 pm", "06:00 pm", "08:00 pm", "10:00 pm", "12:00 am"), breaks =  c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #facet_wrap(~ year) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "11.05.2020_indicesbytimeofday.jpg"))
  
  

#Plotting per hour per site

#indicesbytimeofday +
  #scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(000000, 060000, 120000, 180000, 240000), labels = c("00", "06", "12", "18", "00")) +
  #facet_wrap(PointData ~ .) +
#ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "07.01.2020_indicesbytimeofdaypersite.jpg"))

