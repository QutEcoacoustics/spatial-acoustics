#Plotting indices per hour#
#Marina Scarpelli#
#07.01.2020#

library(tidyverse)
library(ggplot2)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

#Reads the data
df <- read.csv(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "indices_all_AM.csv"))
df_scaled <- mutate_at(df, vars(5:27), scale)
cor <- abs(cor(df_scaled[c(9, 14, 16, 19:23, 25)], use = "complete.obs", method = "spearman")) %>% 
  write.csv(., getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "28.01.2020_correlationmatrix_AMdata3.csv"))

df_Highlycorrelatedremoved <- df_scaled[c(9, 14, 16, 19:23, 25, 27:40)]
write.csv(df_Highlycorrelatedremoved, getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "28.01.2020_AMdata_highlycorrelatedremoved.csv"))

#Putting the indices as a variable
df_wider <- gather(df, c(BackgroundNoise, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfVarianceSpectrum, ClusterCount, Ndsi), key = "Index", value = "value")

df <- mutate(df, time_rec_numer = str_pad(as.integer(time_rec_numer), width = 6, side = "left", pad = "0"))



#Plotting all the sites together

indicesbytimeofday <- ggplot(df_wider, aes(x = time_rec_numer, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "Index Scaled Values") +
  ggtitle("Scaled Indices Values by time of the day (in hours)") +
  facet_wrap(Date~.)
indicesbytimeofday + #scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) +
  
ggsave(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "Figures", "18.01.2020_indicesbytimeofday.jpg"))



#Plotting per hour per site
filtered_byTransect <- filter(df_wider, Transect == "WB")

indicesbytimeofday_persite <- ggplot(filtered_byTransect, aes(x = time, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "Index Scaled Values") +
  ggtitle("Scaled Indices Values by time of the day (in hours)")

indicesbytimeofday_persite +
  scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(000000, 060000, 120000, 180000, 240000), labels = c("00", "06", "12", "18", "00")) +
  facet_wrap(Transectpoint~ .) +
  ggsave(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "Figures", "28.01.2020_indicesbytimeofdaypersite_WB.jpg"))

