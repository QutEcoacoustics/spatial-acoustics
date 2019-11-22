#Comparing the indices values for the two channels
#Marina D. A. Scarpelli
#08.11.2019

#Removing all objects
rm(list = ls())

#Loading necessary packages
library(tidyverse)
library(ggplot2)

#Loading data - separated dataframe per each channel. Also, add a column with information about which channel this analysis is derived from
indices_channel1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/indices_all.csv", row.names = 1) %>% 
  mutate(., "Channel" = "1")

indices_channel2 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/indices_all.csv", row.names = 1)%>% 
  mutate(., "Channel" = "2")

#Rbind both df
indices_all <- rbind(indices_channel1, indices_channel2)

normalise <- function (x, xmin, xmax) {
  y <- (x - xmin)/(xmax - xmin)
}

###########################################################
# Create a normalised dataset between 1.5 and 98.5% bounds 
###########################################################
indices_norm <- indices_all

# normalise values between 1.5 and 98.5 percentile bounds
q1.values <- NULL
q2.values <- NULL
for (i in 1:ncol(indices_all[1:15])) {
  q1 <- unname(quantile(indices_all[,i], probs = 0.015, na.rm = TRUE))
  q2 <- unname(quantile(indices_all[,i], probs = 0.985, na.rm = TRUE))
  q1.values <- c(q1.values, q1)
  q2.values <- c(q2.values, q2)
  indices_norm[,i]  <- normalise(indices_all[,i], q1, q2)
}
rm(q1, q2, i)

# adjust values greater than 1 or less than 0 to 1 and 0 respectively
for (j in 1:ncol(indices_norm[1:15])) {
  a <- which(indices_norm[,j] > 1)
  indices_norm[a,j] = 1
  a <- which(indices_norm[,j] < 0)
  indices_norm[a,j] = 0
}
##############################################
# Correlation matrix (Summary Indices) of thirteen months (398 days) at two sites
##############################################

#Normality plots
library(ggpubr)

BGN <- ggqqplot(indices_all$BackgroundNoise, ylab = "BGN", title = "BGN normality plot")
SNR <- ggqqplot(indices_all$Snr, ylab = "SNR", title = "SNR normality plot")
ACT <- ggqqplot(indices_all$Activity, ylab = "ACT", title = "ACT normality plot")
EVN <- ggqqplot(indices_all$EventsPerSecond, ylab = "EVN", title = "EVN normality plot")
HFC <- ggqqplot(indices_all$HighFreqCover, ylab = "HFC", title = "HFC normality plot")
MFC <- ggqqplot(indices_all$MidFreqCover, ylab = "MFC", title = "MFC normality plot")
LFC <- ggqqplot(indices_all$LowFreqCover, ylab = "LFC", title = "LFC normality plot")
ACI <- ggqqplot(indices_all$AcousticComplexity, ylab = "ACI", title = "ACI normality plot")
ENT <- ggqqplot(indices_all$TemporalEntropy, ylab = "ENT", title = "ENT normality plot")
EAS <- ggqqplot(indices_all$EntropyOfAverageSpectrum, ylab = "EAS", title = "EAS normality plot")
EPS <- ggqqplot(indices_all$EntropyOfPeaksSpectrum, ylab = "EPS", title = "EPS normality plot")
ECV <- ggqqplot(indices_all$EntropyOfVarianceSpectrum, ylab = "ECV", title = "ECV normality plot")
CLC <- ggqqplot(indices_all$ClusterCount, ylab = "CLC", title = "CLC normality plot")
SPD <- ggqqplot(indices_all$SptDensity, ylab = "SPD", title = "SPD normality plot")

ggexport(BGN, SNR, ACT, EVN, HFC, MFC, LFC, ACI, ENT, EAS, EPS, ECV, CLC, SPD, filename = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ChannelComparison/NormalityPlot.tiff")

rm(BGN, SNR, ACT, EVN, HFC, MFC, LFC, ACI, ENT, EAS, EPS, ECV, CLC, SPD)

#Correlation Matrix
cor <- abs(cor(indices_all[1:15], use = "complete.obs", method = "spearman"))
##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ChannelComparison/Correlation_matrix.csv")

#Removing highly correlated indices
remove <- c(2, 3, 4, 6, 9, 12, 15)
indices_all <- indices_all[,-remove]
rm(remove)


cor <- abs(cor(indices_all[1:8], use = "complete.obs", method = "spearman"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ChannelComparison/Correlation_matrix1.csv")

#Same analysis above with normalised indices values
normBGN <- ggqqplot(indices_norm$BackgroundNoise, ylab = "BGN", title = "Normalised BGN normality plot")
normSNR <- ggqqplot(indices_norm$Snr, ylab = "SNR", title = "Normalised SNR normality plot")
normACT <- ggqqplot(indices_norm$Activity, ylab = "ACT", title = "Normalised ACT normality plot")
normEVN <- ggqqplot(indices_norm$EventsPerSecond, ylab = "EVN", title = "Normalised EVN normality plot")
normHFC <- ggqqplot(indices_norm$HighFreqCover, ylab = "HFC", title = "Normalised HFC normality plot")
normMFC <- ggqqplot(indices_norm$MidFreqCover, ylab = "MFC", title = "Normalised MFC normality plot")
normLFC <- ggqqplot(indices_norm$LowFreqCover, ylab = "LFC", title = "Normalised LFC normality plot")
normACI <- ggqqplot(indices_norm$AcousticComplexity, ylab = "ACI", title = "Normalised ACI normality plot")
normENT <- ggqqplot(indices_norm$TemporalEntropy, ylab = "ENT", title = "Normalised ENT normality plot")
normEAS <- ggqqplot(indices_norm$EntropyOfAverageSpectrum, ylab = "EAS", title = "Normalised EAS normality plot")
normEPS <- ggqqplot(indices_norm$EntropyOfPeaksSpectrum, ylab = "EPS", title = "Normalised EPS normality plot")
normECV <- ggqqplot(indices_norm$EntropyOfVarianceSpectrum, ylab = "ECV", title = "Normalised ECV normality plot")
normCLC <- ggqqplot(indices_norm$ClusterCount, ylab = "CLC", title = "Normalised CLC normality plot")
normSPD <- ggqqplot(indices_norm$SptDensity, ylab = "SPD", title = "Normalised SPD normality plot")

ggexport(normBGN, normSNR, normACT, normEVN, normHFC, normMFC, normLFC, normACI, normENT, normEAS, normEPS, normECV, normCLC, normSPD, filename = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ChannelComparison/NormalisedNormalityPlot.tiff")
rm(normBGN, normSNR, normACT, normEVN, normHFC, normMFC, normLFC, normACI, normENT, normEAS, normEPS, normECV, normCLC, normSPD)

#Data is not normally distributed, so I used spearman rank correlation#

cor <- abs(cor(df_all[1:15], use = "complete.obs", method = "spearman"))
d <- pairs(df_all[1:15]) 
  

remove <- c(2, 3, 4, 6, 9, 12, 15)
indices_norm <- indices_norm[,-remove]

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ChannelComparison/Correlation_matrix_NormaValues.csv")
rm(cor)

#Comparing between mics
ggplot(indices_norm, aes(x=Channel, y=Ndsi)) + 
  geom_violin() +
ggsave("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ChannelComparison/ViolinPlots/NormNDSI.tiff")


#Test: is there a significant difference between indices medians of the two channels? Explanatory variable: channel - discrete; Resposta: Indice - continuous;




