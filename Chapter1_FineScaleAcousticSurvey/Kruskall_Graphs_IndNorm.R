#Graphs - Soundland
#Marina Scarpelli
#28 of June 2019

library(tidyverse)
library(ggplot2)
library(FSA)
library(ggplot2)
library(dunn.test)
library(purrr)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra",  ...))
}


rm(list = ls())

#Get data#####
df <- read.csv(getDataPath("Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation.csv"), row.names = 3)

df_wider <- gather(df, c(BackgroundNoise, HighFreqCover, LowFreqCover, AcousticComplexity, EntropyOfVarianceSpectrum, ClusterCount, Ndsi), key = "Index", value = "value")

#Background noise
kruskal.test(df$BackgroundNoise~df$PointData)
dunn.test(df$BackgroundNoise, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$BackgroundNoise, df$PointData, p.adj = "bonferroni")

#High Freq Cover
kruskal.test(df$HighFreqCover~df$PointData)
dunn.test(df$HighFreqCover, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$HighFreqCover, df$PointData, p.adj = "bonferroni")

#Low Freq Cover
kruskal.test(df$LowFreqCover~df$PointData)
dunn.test(df$LowFreqCover, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$LowFreqCover, df$PointData, p.adj = "bonferroni")

#Acoustic Complexity
kruskal.test(df$AcousticComplexity~df$PointData)
dunn.test(df$AcousticComplexity, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$AcousticComplexity, df$PointData, p.adj = "bonferroni")

#Cluster Count
kruskal.test(df$ClusterCount~df$PointData)
dunn.test(df$ClusterCount, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$ClusterCount, df$PointData, p.adj = "bonferroni")

#Cluster Count
kruskal.test(df$EntropyOfVarianceSpectrum~df$PointData)
dunn.test(df$EntropyOfVarianceSpectrum, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$EntropyOfVarianceSpectrum, df$PointData, p.adj = "bonferroni")

#NDSI
kruskal.test(df$Ndsi~df$PointData)
dunn.test(df$Ndsi, df$PointData, method = "bonferroni")
pairwise.wilcox.test(df$Ndsi, df$PointData, p.adj = "bonferroni")


#Violin Plots##
#In colour - Indices per Site
  ggplot(df_wider, aes(x = Index, y = value)) +
  geom_violin(aes(fill = df_wider$Index)) +
  facet_wrap(df_wider$PointData~., scales = "free", ncol = 4) +
  labs(x = element_blank(), y = "Scale Index Values") +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Indices")) +
  scale_fill_discrete(labels = c("Acoustic Complexity", "Backrgound Noise", "Cluster Count", "Entropy of Variance Spectrum", "High Frequency Cover", "Low Frequency Cover", "NDSI"))
 ggsave(getDataPath("Oct2019", "Figures", "30.01.2020_ViolinPlot_IndicesPerSite.jpg"), dpi = 300)
 

library(ggpubr)
 
#Violin Plots##
#In colour 
 ggplot(df, aes(PointData, HighFreqCover)) +
   geom_violin(aes(fill = df$PointData), draw_quantiles = 0.5) +
   labs(x = element_blank(), y = "Scale Index Values", title = "Background Noise per Site") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5),
         legend.position = "none") +
   guides(fill = guide_legend(title="Point")) #+
 #ggsave(getDataPath("Oct2019", "Figures", "30.01.2020_ViolinPlot_IndicesPerSite.jpg"), dpi = 300)



#Signal to noise/ENV####
kruskal.test(df$Snr~df$ENV)
dunn.test(df$Snr, df$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$Snr, df1$ENV, p.adj = "bonferroni")

#Activity/ENV####
kruskal.test(df1$Activity~df1$ENV)
dunn.test(df1$Activity, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$Activity, df1$ENV, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$ENV, y = df1$Activity)) +
  geom_violin(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Activity", title = "Activity (ACT) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment"))
  #ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACT_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Activity", title = "Activity (ACT) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACT_ENV_col.jpg", dpi = 300)

#Events per Second/ENV####
kruskal.test(df1$EventsPerSecond~df1$ENV)
dunn.test(df1$EventsPerSecond, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$EventsPerSecond, df1$ENV, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$ENV, y = df1$EventsPerSecond)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Events per Second", title = "Events per Second (EVN) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EVN_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$EventsPerSecond)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Events per Second", title = "Events per Second (EVN) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EVN_ENV_col.jpg", dpi = 300)

#LFC/ENV####
kruskal.test(df1$LowFreqCover~df1$ENV)
dunn.test(df1$LowFreqCover, df1$ENV, method = "bonferroni")

#MFC/ENV####
kruskal.test(df1$MidFreqCover~df1$ENV)
dunn.test(df1$MidFreqCover, df1$ENV, method = "bonferroni")

#HFC/ENV####
kruskal.test(df1$HighFreqCover~df1$ENV)
dunn.test(df1$HighFreqCover, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$HighFreqCover, df1$ENV, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$ENV, y = df1$HighFreqCover)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "High Frequency Cover", title = "High Frequency Cover (HFC) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_HFC_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$HighFreqCover)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "High Frequency Cover", title = "High Frequency Cover (HFC) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_HFC_ENV_col.jpg", dpi = 300)


#ENT/ENV####
kruskal.test(df1$TemporalEntropy~df1$ENV)
dunn.test(df1$TemporalEntropy, df1$ENV, method = "bonferroni")

#EPS/ENV####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$ENV)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$EntropyOfPeaksSpectrum, df1$ENV, p.adj = "bonferroni")

ggplot(data = df1, aes(x = df1$ENV, y = df1$EntropyOfPeaksSpectrum)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Entropy of Peaks Spectrum", title = "Entropy of Peaks Spectrum (EPS) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EPS_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$EntropyOfPeaksSpectrum)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Entropy of Peaks Spectrum", title = "Entropy of Peaks Spectrum (EPS) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EPS_ENV_col.jpg", dpi = 300)

#EAS/ENV####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$ENV)
dunn.test(df1$EntropyOfAverageSpectrum, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$EntropyOfAverageSpectrum, df1$ENV, p.adj = "bonferroni")

#Boxplot##

ggplot(data = df1, aes(x = df1$ENV, y = df1$EntropyOfAverageSpectrum)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Entropy Of Average Spectrum", title = "Entropy Of Average Spectrum (EAS) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EAS_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$EntropyOfAverageSpectrum)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Entropy Of Average Spectrum", title = "Entropy of Average Spectrum (EAS) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EAS_ENV_col.jpg", dpi = 300)

#ECV/ENV####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$ENV)
dunn.test(df1$EntropyOfCoVSpectrum, df1$ENV, method = "bonferroni")

#ACI/ENV####
kruskal.test(df1$AcousticComplexity~df1$ENV)
dunn.test(df1$AcousticComplexity, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$AcousticComplexity, df1$ENV, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$ENV, y = df1$AcousticComplexity)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Acoustic Complexity", title = "Acoustic Complexity (ACI) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.3), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACI_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$AcousticComplexity)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Acoustic Complexity", title = "Acoustic Complexity (ACI) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.3), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACI_ENV_col.jpg", dpi = 300)

#SPD/ENV####
kruskal.test(df1$ClusterCount~df1$ENV)

#SPD/ENV####
kruskal.test(df1$SptDensity~df1$ENV)
dunn.test(df1$SptDensity, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$SptDensity, df1$ENV, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$ENV, y = df1$SptDensity)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Spectral Peak Density", title = "Spectral Peak Density (SPD) levels by environment") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_SPD_ENV_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$ENV, y = df1$SptDensity)) +
  geom_boxplot(aes(fill = df1$ENV)) +
  labs(x = element_blank(), y = "Spectral Peak Density", title = "Spectral Peak Density (SPD) levels by environment") +
  scale_fill_manual(values = c("#4d9221", "#8c510a", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left") +
  guides(fill = guide_legend(title="Environment")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_SPD_ENV_col.jpg", dpi = 300)

#CLS/ENV####
kruskal.test(df1$ClusterCount~df1$ENV)
dunn.test(df1$ClusterCount, df1$ENV, method = "bonferroni")
pairwise.wilcox.test(df1$SptDensity, df1$ENV, p.adj = "bonferroni")


#NDSI/ENV####

kruskal.test(df1$Ndsi~df1$ENV)
dunn.test(df1$Ndsi, df1$ENV, method = "bonferroni")

#NATURAL CLASSIFICATION####
#Background noise/Buffer_100m####
kruskal.test(df1$BackgroundNoise~df1$X100_class_nat)
dunn.test(df1$BackgroundNoise, df1$X100_class_nat, method = "bonferroni")

#Background noise/Buffer_200m####
kruskal.test(df1$BackgroundNoise~df1$X200_class_nat)
dunn.test(df1$BackgroundNoise, df1$X200_class_nat, method = "bonferroni")

#Background noise/Buffer_500m####
kruskal.test(df1$BackgroundNoise~df1$X500_class_nat)
dunn.test(df1$BackgroundNoise, df1$X500_class_nat, method = "bonferroni")

#Background noise/Buffer_1km####
kruskal.test(df1$BackgroundNoise~df1$X1k_class_nat)
dunn.test(df1$BackgroundNoise, df1$X1k_class_nat, method = "bonferroni")

#Background noise/Buffer_2km####
kruskal.test(df1$BackgroundNoise~df1$X2K_class_nat)
dunn.test(df1$BackgroundNoise, df1$X2K_class_nat, method = "bonferroni")

#Nada significativo no BGN#

#Signal to noise/Buffer_100m####
kruskal.test(df1$Snr~df1$X100_class_nat)
dunn.test(df1$Snr, df1$X100_class_nat, method = "bonferroni")

#Signal to noise/Buffer_200m####
kruskal.test(df1$Snr~df1$X200_class_nat)
dunn.test(df1$Snr, df1$X200_class_nat, method = "bonferroni")

#Signal to noise/Buffer_500m####
kruskal.test(df1$Snr~df1$X500_class_nat)
dunn.test(df1$Snr, df1$X500_class_nat, method = "bonferroni")

#Signal to noise/Buffer_1km####
kruskal.test(df1$Snr~df1$X1k_class_nat)
dunn.test(df1$Snr, df1$X1k_class_nat, method = "bonferroni")

#Signal to noise/Buffer_2km####
kruskal.test(df$Snr~df$X2K_class_nat)
dunn.test(df$Snr, df$X2K_class_nat, method = "bonferroni")
pairwise.wilcox.test(df$Snr, df$X2K_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW

ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$Snr)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Signal to Noise Ratio", title = "SNR levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_Snr2k_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$Snr)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Signal to Noise Ratio", title = "SNR levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_Snr2k_nat_cl.jpg", dpi = 300)

#Activity/Buffer_100m####
kruskal.test(df1$Activity~df1$X100_class_nat)
dunn.test(df1$Activity, df1$X100_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$Activity, df1$X100_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Activity", title = "ACT levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ACT100m_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Activity", title = "ACT levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ACT100_nat_cl.jpg", dpi = 300)
#Significativo p/ 100m#

#Activity/Buffer_200m####
kruskal.test(df1$Activity~df1$X200_class_nat)
dunn.test(df1$Activity, df1$X200_class_nat, method = "bonferroni")

#Activity/Buffer_500m####
kruskal.test(df1$Activity~df1$X500_class_nat)
dunn.test(df1$Activity, df1$X500_class_nat, method = "bonferroni")

#Activity/Buffer_1km####
kruskal.test(df1$Activity~df1$X1k_class_nat)
dunn.test(df1$Activity, df1$X1k_class_nat, method = "bonferroni")

#Activity/Buffer_2km####
kruskal.test(df1$Activity~df1$X2K_class_nat)
dunn.test(df1$Activity, df1$X2K_class_nat, method = "bonferroni")

#Events per Second/Buffer_100m####
kruskal.test(df1$EventsPerSecond~df1$X100_class_nat)
dunn.test(df1$EventsPerSecond, df1$X100_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$EventsPerSecond, df1$X100_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$EventsPerSecond)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Events per Second", title = "EVN levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_EVN100_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$EventsPerSecond)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Events per Second", title = "EVN levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_EVN100_nat_cl.jpg", dpi = 300)

#Events per Second/Buffer_200m####
kruskal.test(df1$EventsPerSecond~df1$X200_class_nat)
dunn.test(df1$Activity, df1$X200_class_nat, method = "bonferroni")

#Events Per Second/Buffer_500m####
kruskal.test(df1$EventsPerSecond~df1$X500_class_nat)
dunn.test(df1$Activity, df1$X500_class_nat, method = "bonferroni")

#Events Per Second/Buffer_1km####
kruskal.test(df1$EventsPerSecond~df1$X1k_class_nat)
dunn.test(df1$Activity, df1$X1k_class_nat, method = "bonferroni")

#Events Per Second/Buffer_2km####
kruskal.test(df1$EventsPerSecond~df1$X2K_class_nat)
dunn.test(df1$Activity, df1$X2K_class_nat, method = "bonferroni")

#LFC/Buffer_100m####
kruskal.test(df1$LowFreqCover~df1$X100_class_nat)
dunn.test(df1$LowFreqCover, df1$X100_class_nat, method = "bonferroni")

#LFC/Buffer_200m####
kruskal.test(df1$LowFreqCover~df1$X200_class_nat)
dunn.test(df1$LowFreqCover, df1$X200_class_nat, method = "bonferroni")

#LFC/Buffer_500m####
kruskal.test(df1$LowFreqCover~df1$X500_class_nat)
dunn.test(df1$LowFreqCover, df1$X500_class_nat, method = "bonferroni")

#LFC/Buffer_1km####
kruskal.test(df1$LowFreqCover~df1$X1k_class_nat)
dunn.test(df1$LowFreqCover, df1$X1k_class_nat, method = "bonferroni")

#LFC/Buffer_2km####
kruskal.test(df1$LowFreqCover~df1$X2K_class_nat)
dunn.test(df1$LowFreqCover, df1$X2K_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$LowFreqCover, df1$X2K_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$LowFreqCover)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Low Frequency Cover", title = "LFC levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_LFC2k_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$LowFreqCover)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Low Frequency Cover", title = "LFC levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_LFC2k_nat_cl.jpg", dpi = 300)

#MFC/Buffer_100m####
kruskal.test(df1$MidFreqCover~df1$X100_class_nat)
dunn.test(df1$MidFreqCover, df1$X100_class_nat, method = "bonferroni")

#MFC/Buffer_200m####
kruskal.test(df1$MidFreqCover~df1$X200_class_nat)
dunn.test(df1$MidFreqCover, df1$X200_class_nat, method = "bonferroni")

#MFC/Buffer_500m####
kruskal.test(df1$MidFreqCover~df1$X500_class_nat)
dunn.test(df1$MidFreqCover, df1$X500_class_nat, method = "bonferroni")

#MFC/Buffer_1km####
kruskal.test(df1$MidFreqCover~df1$X1k_class_nat)
dunn.test(df1$MidFreqCover, df1$X1k_class_nat, method = "bonferroni")

#MFC/Buffer_2km####
kruskal.test(df1$MidFreqCover~df1$X2K_class_nat)
dunn.test(df1$MidFreqCover, df1$X2K_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$MidFreqCover, df1$X2K_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$MidFreqCover)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Mid Frequency Cover", title = "MFC levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_MFC2k_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$MidFreqCover)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Mid Frequency Cover", title = "MFC levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_MFC2k_nat_cl.jpg", dpi = 300)


#HFC/Buffer_100m####
kruskal.test(df1$HighFreqCover~df1$X100_class_nat)
dunn.test(df1$HighFreqCover, df1$X100_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$HighFreqCover, df1$X100_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$HighFreqCover)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "High Frequency Cover", title = "HFC levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_HFC100_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$HighFreqCover)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "High Frequency Cover", title = "HFC levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_HFC100_nat_cl.jpg", dpi = 300)


#HFC/Buffer_200m####
kruskal.test(df1$HighFreqCover~df1$X200_class_nat)
dunn.test(df1$HighFreqCover, df1$X200_class_nat, method = "bonferroni")

#HFC/Buffer_500m####
kruskal.test(df1$HighFreqCover~df1$X500_class_nat)
dunn.test(df1$HighFreqCover, df1$X500_class_nat, method = "bonferroni")

#HFC/Buffer_1km####
kruskal.test(df1$HighFreqCover~df1$X1k_class_nat)
dunn.test(df1$HighFreqCover, df1$X1k_class_nat, method = "bonferroni")

#HFC/Buffer_2km####
kruskal.test(df1$HighFreqCover~df1$X2K_class_nat)
dunn.test(df1$HighFreqCover, df1$X2K_class_nat, method = "bonferroni")

#ENT/Buffer_100m####
kruskal.test(df1$TemporalEntropy~df1$X100_class_nat)
dunn.test(df1$TemporalEntropy, df1$X100_class_nat, method = "bonferroni")

#ENT/Buffer_200m####
kruskal.test(df1$TemporalEntropy~df1$X200_class_nat)
dunn.test(df1$TemporalEntropy, df1$X200_class_nat, method = "bonferroni")

#ENT/Buffer_500m####
kruskal.test(df1$TemporalEntropy~df1$X500_class_nat)
dunn.test(df1$TemporalEntropy, df1$X500_class_nat, method = "bonferroni")

#ENT/Buffer_1km####
kruskal.test(df1$TemporalEntropy~df1$X1k_class_nat)
dunn.test(df1$TemporalEntropy, df1$X1k_class_nat, method = "bonferroni")

#ENT/Buffer_2km####
kruskal.test(df1$TemporalEntropy~df1$X2K_class_nat)
dunn.test(df1$TemporalEntropy, df1$X2K_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$TemporalEntropy, df1$X2K_class_nat, p.adj = "bonferroni")

ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$TemporalEntropy)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Temporal Entropy", title = "ENT levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ENT2K_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$TemporalEntropy)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Temporal Entropy", title = "ENT levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ENT2K_nat_cl.jpg", dpi = 300)

#EPS/Buffer_100m####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X100_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X100_class_nat, method = "bonferroni")

#EPS/Buffer_200m####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X200_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X200_class_nat, method = "bonferroni")

#EPS/Buffer_500m####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X500_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X500_class_nat, method = "bonferroni")

#EPS/Buffer_1km####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X1k_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X1k_class_nat, method = "bonferroni")

#EPS/Buffer_2km####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X2K_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X2K_class_nat, method = "bonferroni")

#EAS/Buffer_100m####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X100_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X100_class_nat, method = "bonferroni")

#EAS/Buffer_200m####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X200_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X200_class_nat, method = "bonferroni")

#EAS/Buffer_500m####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X500_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X500_class_nat, method = "bonferroni")

#EAS/Buffer_1km####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X1k_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X1k_class_nat, method = "bonferroni")

#EAS/Buffer_2km####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X2K_class_nat)
dunn.test(df1$EntropyOfPeaksSpectrum, df1$X2K_class_nat, method = "bonferroni")

#ECV/Buffer_100m####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X100_class_nat)
dunn.test(df1$EntropyOfCoVSpectrum, df1$X100_class_nat, method = "bonferroni")

#ECV/Buffer_200m####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X200_class_nat)
dunn.test(df1$EntropyOfCoVSpectrum, df1$X200_class_nat, method = "bonferroni")

#ECV/Buffer_500m####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X500_class_nat)
dunn.test(df1$EntropyOfCoVSpectrum, df1$X500_class_nat, method = "bonferroni")

#ECV/Buffer_1km####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X1k_class_nat)
dunn.test(df1$EntropyOfCoVSpectrum, df1$X1k_class_nat, method = "bonferroni")

#ECV/Buffer_2km####
kruskal.test(df$EntropyOfCoVSpectrum~df$X2K_class_nat)
dunn.test(df$EntropyOfCoVSpectrum, df$X2K_class_nat, method = "bonferroni")
pairwise.wilcox.test(df$EntropyOfCoVSpectrum, df$X2K_class_nat, p.adj = "bonferroni")

ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$EntropyOfCoVSpectrum)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Entropy of Covariance Spectrum", title = "ECV levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ECV2K_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$EntropyOfCoVSpectrum)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Entropy of Covariance Spectrum", title = "ECV levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ECV2K_nat_cl.jpg", dpi = 300)


#ACI/Buffer_100m####
kruskal.test(df1$AcousticComplexity~df1$X100_class_nat)
dunn.test(df1$AcousticComplexity, df1$X100_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$AcousticComplexity, df1$X100_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$AcousticComplexity)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Acoustic Complexity", title = "ACI levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ACI100_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$AcousticComplexity)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Acoustic Complexity", title = "ACI levels by Naturalness Habitat Classification (Buffer size: 100m)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_ACI100_nat_cl.jpg", dpi = 300)


#ACI/Buffer_200m####
kruskal.test(df1$AcousticComplexity~df1$X200_class_nat)
dunn.test(df1$AcousticComplexity, df1$X200_class_nat, method = "bonferroni")

#ACI/Buffer_500m####
kruskal.test(df1$AcousticComplexity~df1$X500_class_nat)
dunn.test(df1$AcousticComplexity, df1$X500_class_nat, method = "bonferroni")

#ACI/Buffer_1km####
kruskal.test(df1$AcousticComplexity~df1$X1k_class_nat)
dunn.test(df1$AcousticComplexity, df1$X1k_class_nat, method = "bonferroni")

#ACI/Buffer_2km####
kruskal.test(df1$AcousticComplexity~df1$X2K_class_nat)
dunn.test(df1$AcousticComplexity, df1$X2K_class_nat, method = "bonferroni")

#CLS/Buffer_100m####
kruskal.test(df1$ClusterCount~df1$X100_class_nat)
dunn.test(df1$ClusterCount, df1$X100_class_nat, method = "bonferroni")

#CLS/Buffer_200m####
kruskal.test(df1$ClusterCount~df1$X200_class_nat)
dunn.test(df1$ClusterCount, df1$X200_class_nat, method = "bonferroni")

#CLS/Buffer_500m####
kruskal.test(df1$ClusterCount~df1$X500_class_nat)
dunn.test(df1$ClusterCount, df1$X500_class_nat, method = "bonferroni")

#CLS/Buffer_1km####
kruskal.test(df1$ClusterCount~df1$X1k_class_nat)
dunn.test(df1$ClusterCount, df1$X1k_class_nat, method = "bonferroni")

#CLS/Buffer_2km####
kruskal.test(df1$ClusterCount~df1$X2K_class_nat)
dunn.test(df1$ClusterCount, df1$X2K_class_nat, method = "bonferroni")
pairwise.wilcox.test(df1$ClusterCount, df1$X2K_class_nat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$ClusterCount)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Cluster Count", title = "CLS levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  guides(fill = guide_legend(title="Habitat Class")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_CLS2K_nat_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X2K_class_nat, y = df1$ClusterCount)) +
  geom_boxplot(aes(fill = df1$X2K_class_nat)) +
  labs(x = "Habitat Classification", y = "Cluster Count", title = "CLS levels by Naturalness Habitat Classification (Buffer size: 2Km)") +
  scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c")) +
  theme_minimal() +
  theme(axis.text.x=) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  guides(fill = guide_legend(title="Habitat Class")) +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/3BoxPlot_CLS2K_nat_cl.jpg", dpi = 300)

#SPD/Buffer_100m####
kruskal.test(df1$SptDensity~df1$X100_class_nat)
dunn.test(df1$SptDensity, df1$X100_class_nat, method = "bonferroni")

#SPD/Buffer_200m####
kruskal.test(df1$SptDensity~df1$X200_class_nat)
dunn.test(df1$SptDensity, df1$X200_class_nat, method = "bonferroni")

#SPD/Buffer_500m####
kruskal.test(df1$SptDensity~df1$X500_class_nat)
dunn.test(df1$SptDensity, df1$X500_class_nat, method = "bonferroni")

#SPD/Buffer_1km####
kruskal.test(df1$SptDensity~df1$X1k_class_nat)
dunn.test(df1$SptDensity, df1$X1k_class_nat, method = "bonferroni")

#SPD/Buffer_2km####
kruskal.test(df1$SptDensity~df1$X2K_class_nat)
dunn.test(df1$SptDensity, df1$X2K_class_nat, method = "bonferroni")

#NDSI/Buffer_100m####
kruskal.test(df1$Ndsi~df1$X100_class_nat)
dunn.test(df1$Ndsi, df1$X100_class_nat, method = "bonferroni")

#NDSI/Buffer_200m####
kruskal.test(df1$Ndsi~df1$X200_class_nat)
dunn.test(df1$Ndsi, df1$X200_class_nat, method = "bonferroni")

#NDSI/Buffer_500m####
kruskal.test(df1$Ndsi~df1$X500_class_nat)
dunn.test(df1$Ndsi, df1$X500_class_nat, method = "bonferroni")

#NDSI/Buffer_1km####
kruskal.test(df1$Ndsi~df1$X1k_class_nat)
dunn.test(df1$Ndsi, df1$X1k_class_nat, method = "bonferroni")

#NDSI/Buffer_2km####
kruskal.test(df1$Ndsi~df1$X2K_class_nat)
dunn.test(df1$Ndsi, df1$X2K_class_nat, method = "bonferroni")


#ANTHROPIC CLASSIFICATION####
#Background noise/Buffer_100m####
kruskal.test(df1$BackgroundNoise~df1$X100_class_habitat)

#Background noise/Buffer_200m####
kruskal.test(df1$BackgroundNoise~df1$X200_class_habitat)

#Background noise/Buffer_500m####
kruskal.test(df1$BackgroundNoise~df1$X500_class_habitat)

#Background noise/Buffer_1km####
kruskal.test(df1$BackgroundNoise~df1$X1k_class_habitat)

#Background noise/Buffer_2km####
kruskal.test(df1$BackgroundNoise~df1$X2K_class_habitat)
#Nada significativo no BGN#

#Signal to noise/Buffer_100m####
kruskal.test(df1$Snr~df1$X100_class_habitat)

#Signal to noise/Buffer_200m####
kruskal.test(df1$Snr~df1$X200_class_habitat)

#Signal to noise/Buffer_500m####
kruskal.test(df1$Snr~df1$X500_class_habitat)

#Signal to noise/Buffer_1km####
kruskal.test(df1$Snr~df1$X1k_class_habitat)

#Signal to noise/Buffer_2km####
kruskal.test(df1$Snr~df1$X2K_class_habitat)
#Nada Significativo no SNR#

#Activity/Buffer_100m####
kruskal.test(df1$Activity~df1$X100_class_habitat)
pairwise.wilcox.test(df1$Activity, df1$X100_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Activity", title = "Activity (ACT) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACTBuffer100_ant_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Activity", title = "Activity (ACT) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACTBuffer100_ant_col.jpg", dpi = 300)
#Significativo p/ 100m#

#Activity/Buffer_200m####
kruskal.test(df1$Activity~df1$X200_class_habitat)

#Activity/Buffer_500m####
kruskal.test(df1$Activity~df1$X500_class_habitat)
pairwise.wilcox.test(df1$Activity, df1$X500_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X500_class_habitat, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$X500_class_habitat)) +
  labs(x = "Habitat Classification", y = "Activity", title = "Activity (ACT) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACTBuffer500_ant_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X500_class_habitat, y = df1$Activity)) +
  geom_boxplot(aes(fill = df1$X500_class_habitat)) +
  labs(x = "Habitat Classification", y = "Activity", title = "Activity (ACT) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACTBuffer500_ant_col.jpg", dpi = 300)

#Activity/Buffer_1km####
kruskal.test(df1$Activity~df1$X1k_class_habitat)

#Activity/Buffer_2km####
kruskal.test(df1$Activity~df1$X2K_class_habitat)

#Events per Second/Buffer_100m####
kruskal.test(df1$EventsPerSecond~df1$X100_class_habitat)
pairwise.wilcox.test(df1$EventsPerSecond, df1$X100_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$EventsPerSecond)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Events per Second", title = "Events per Second (EPS) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EPSBuffer100_ANT_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$EventsPerSecond)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Events per Second", title = "Events per Second (EPS) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_EPSBuffer100_ANT_col.jpg", dpi = 300)

#Events per Second/Buffer_200m####
kruskal.test(df1$EventsPerSecond~df1$X200_class_habitat)

#EventsPerSecond/Buffer_500m####
kruskal.test(df1$EventsPerSecond~df1$X500_class_habitat)

#Signal to noise/Buffer_1km####
kruskal.test(df1$EventsPerSecond~df1$X1k_class_habitat)

#EventsPerSecond/Buffer_2km####
kruskal.test(df1$EventsPerSecond~df1$X2K_class_habitat)


#LFC/Buffer_100m####
kruskal.test(df1$LowFreqCover~df1$X100_class_habitat)

#LFC/Buffer_200m####
kruskal.test(df1$LowFreqCover~df1$X200_class_habitat)

#LFC/Buffer_500m####
kruskal.test(df1$LowFreqCover~df1$X500_class_habitat)

#LFC/Buffer_1km####
kruskal.test(df1$LowFreqCover~df1$X1k_class_habitat)

#LFC/Buffer_2km####
kruskal.test(df1$LowFreqCover~df1$X2K_class_habitat)

#MFC/Buffer_100m####
kruskal.test(df1$MidFreqCover~df1$X100_class_habitat)

#MFC/Buffer_200m####
kruskal.test(df1$MidFreqCover~df1$X200_class_habitat)

#MFC/Buffer_500m####
kruskal.test(df1$MidFreqCover~df1$X500_class_habitat)

#MFC/Buffer_1km####
kruskal.test(df1$MidFreqCover~df1$X1k_class_habitat)

#MFC/Buffer_2km####
kruskal.test(df1$MidFreqCover~df1$X2K_class_habitat)

#HFC/Buffer_100m####
kruskal.test(df1$HighFreqCover~df1$X100_class_habitat)
pairwise.wilcox.test(df1$HighFreqCover, df1$X100_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$HighFreqCover)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "High Frequency Cover", title = "High Frequency Cover (HFC) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_HFCBuffer100_ANT_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$HighFreqCover)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "High Frequency Cover", title = "High Frequency Cover (HFC) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_HFCBuffer100_ANT_col.jpg", dpi = 300)

#HFC/Buffer_200m####
kruskal.test(df1$HighFreqCover~df1$X200_class_habitat)

#HFC/Buffer_500m####
kruskal.test(df1$HighFreqCover~df1$X500_class_habitat)

#HFC/Buffer_1km####
kruskal.test(df1$HighFreqCover~df1$X1k_class_habitat)

#HFC/Buffer_2km####
kruskal.test(df1$HighFreqCover~df1$X2K_class_habitat)

#ENT/Buffer_100m####
kruskal.test(df1$TemporalEntropy~df1$X100_class_habitat)

#ENT/Buffer_200m####
kruskal.test(df1$TemporalEntropy~df1$X200_class_habitat)

#ENT/Buffer_500m####
kruskal.test(df1$TemporalEntropy~df1$X500_class_habitat)

#ENT/Buffer_1km####
kruskal.test(df1$TemporalEntropy~df1$X1k_class_habitat)

#ENT/Buffer_2km####
kruskal.test(df1$TemporalEntropy~df1$X2K_class_habitat)

#EPS/Buffer_100m####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X100_class_habitat)

#EPS/Buffer_200m####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X200_class_habitat)

#EPS/Buffer_500m####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X500_class_habitat)

#EPS/Buffer_1km####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X1k_class_habitat)

#EPS/Buffer_2km####
kruskal.test(df1$EntropyOfPeaksSpectrum~df1$X2K_class_habitat)

#EAS/Buffer_100m####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X100_class_habitat)

#EAS/Buffer_200m####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X200_class_habitat)

#EAS/Buffer_500m####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X500_class_habitat)

#EAS/Buffer_1km####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X1k_class_habitat)

#EAS/Buffer_2km####
kruskal.test(df1$EntropyOfAverageSpectrum~df1$X2K_class_habitat)

#ECV/Buffer_100m####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X100_class_habitat)

#ECV/Buffer_200m####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X200_class_habitat)

#ECV/Buffer_500m####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X500_class_habitat)

#ECV/Buffer_1km####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X1k_class_habitat)

#ECV/Buffer_2km####
kruskal.test(df1$EntropyOfCoVSpectrum~df1$X2K_class_habitat)

#ACI/Buffer_100m####
kruskal.test(df1$AcousticComplexity~df1$X100_class_habitat)
pairwise.wilcox.test(df1$AcousticComplexity, df1$X100_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$AcousticComplexity)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Acoustic Complexity", title = "Acoustic Complexity (ACI) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACIBuffer100_ANT_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_nat, y = df1$AcousticComplexity)) +
  geom_boxplot(aes(fill = df1$X100_class_nat)) +
  labs(x = "Habitat Classification", y = "Acoustic Complexity", title = "Acoustic Complexity (ACI) levels by Anthropic Habitat Classificationn") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_ACIBuffer100_ANT_col.jpg", dpi = 300)


#ACI/Buffer_200m####
kruskal.test(df1$AcousticComplexity~df1$X200_class_habitat)

#ACI/Buffer_500m####
kruskal.test(df1$AcousticComplexity~df1$X500_class_habitat)

#ACI/Buffer_1km####
kruskal.test(df1$AcousticComplexity~df1$X1k_class_habitat)

#ACI/Buffer_2km####
kruskal.test(df1$AcousticComplexity~df1$X2K_class_habitat)

#CLS/Buffer_100m####
kruskal.test(df1$ClusterCount~df1$X100_class_habitat)
pairwise.wilcox.test(df1$ClusterCount, df1$X100_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$ClusterCount)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Cluster Count", title = "Cluster Count (CLS) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_CLSBuffer100_ANT_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$ClusterCount)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Acoustic Complexity", title = "Cluster Count (CLS) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_CLSBuffer100_ANT_col.jpg", dpi = 300)


#CLS/Buffer_200m####
kruskal.test(df1$ClusterCount~df1$X200_class_habitat)

#CLS/Buffer_500m####
kruskal.test(df1$ClusterCount~df1$X500_class_habitat)

#CLS/Buffer_1km####
kruskal.test(df1$ClusterCount~df1$X1k_class_habitat)

#CLS/Buffer_2km####
kruskal.test(df1$ClusterCount~df1$X2K_class_habitat)

#SPD/Buffer_100m####
kruskal.test(df1$SptDensity~df1$X100_class_habitat)
pairwise.wilcox.test(df1$SptDensity, df1$X100_class_habitat, p.adj = "bonferroni")

#Boxplots##
#BW
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$SptDensity)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Cluster Count", title = "Spectral Peak Density (SPD) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_SPDBuffer100_ANT_bw.jpg", dpi = 300)

#In colour##
ggplot(data = df1, aes(x = df1$X100_class_habitat, y = df1$SptDensity)) +
  geom_boxplot(aes(fill = df1$X100_class_habitat)) +
  labs(x = "Habitat Classification", y = "Acoustic Complexity", title = "Spectral Peak Density (SPD) levels by Anthropic Habitat Classification") +
  scale_fill_manual(values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
  ggsave("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/BoxPlots/BoxPlot_SPDBuffer100_ANT_col.jpg", dpi = 300)


#SPD/Buffer_200m####
kruskal.test(df1$SptDensity~df1$X200_class_habitat)

#SPD/Buffer_500m####
kruskal.test(df1$SptDensity~df1$X500_class_habitat)

#SPD/Buffer_1km####
kruskal.test(df1$SptDensity~df1$X1k_class_habitat)

#SPD/Buffer_2km####
kruskal.test(df1$SptDensity~df1$X2K_class_habitat)

#NDSI/Buffer_100m####
kruskal.test(df1$Ndsi~df1$X100_class_habitat)

#NDSI/Buffer_200m####
kruskal.test(df1$Ndsi~df1$X200_class_habitat)

#NDSI/Buffer_500m####
kruskal.test(df1$Ndsi~df1$X500_class_habitat)

#NDSI/Buffer_1km####
kruskal.test(df1$Ndsi~df1$X1k_class_habitat)

#NDSI/Buffer_2km####
kruskal.test(df1$Ndsi~df1$X2K_class_habitat)

