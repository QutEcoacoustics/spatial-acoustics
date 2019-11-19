library(tidyverse)
library(spdep)
library(vegan)
library(adegenet)
library(ape)

df <- read.csv("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/Pontos_ind_metricas.csv", header = TRUE, row.names = 29)
df_Forest <- filter(df, df$ENV == "Forest")
points.dist <- as.matrix(dist(cbind(df_Forest$Y, df_Forest$X)))
points.dist.inv <- (1/points.dist)
diag(points.dist.inv) <- 0

Moran.I(df_Forest$BackgroundNoise, points.dist.inv)

Moran.I(df_Forest$Snr, points.dist.inv)

Moran.I(df_Forest$Activity, points.dist.inv)

Moran.I(df_Forest$EventsPerSecond, points.dist.inv)

Moran.I(df_Forest$LowFreqCover, points.dist.inv)

Moran.I(df_Forest$MidFreqCover, points.dist.inv)

Moran.I(df_Forest$HighFreqCover, points.dist.inv)

Moran.I(df_Forest$TemporalEntropy, points.dist.inv)

Moran.I(df_Forest$EntropyOfAverageSpectrum, points.dist.inv)

Moran.I(df_Forest$EntropyOfPeaksSpectrum, points.dist.inv)

Moran.I(df_Forest$EntropyOfVarianceSpectrum, points.dist.inv)

Moran.I(df_Forest$AcousticComplexity, points.dist.inv)

Moran.I(df_Forest$ClusterCount, points.dist.inv)

Moran.I(df_Forest$SptDensity, points.dist.inv)

df_Open <- filter(df, df$ENV == "Open")
points.dist <- as.matrix(dist(cbind(df_Open$Y, df_Open$X)))
points.dist.inv <- (1/points.dist)
diag(points.dist.inv) <- 0

Moran.I(df_Open$BackgroundNoise, points.dist.inv)

Moran.I(df_Open$Snr, points.dist.inv)

Moran.I(df_Open$Activity, points.dist.inv)

Moran.I(df_Open$EventsPerSecond, points.dist.inv)

Moran.I(df_Open$LowFreqCover, points.dist.inv)

Moran.I(df_Open$MidFreqCover, points.dist.inv)

Moran.I(df_Open$HighFreqCover, points.dist.inv)

Moran.I(df_Open$TemporalEntropy, points.dist.inv)

Moran.I(df_Open$EntropyOfAverageSpectrum, points.dist.inv)

Moran.I(df_Open$EntropyOfPeaksSpectrum, points.dist.inv)

Moran.I(df_Open$EntropyOfVarianceSpectrum, points.dist.inv)

Moran.I(df_Open$AcousticComplexity, points.dist.inv)

Moran.I(df_Open$ClusterCount, points.dist.inv)

Moran.I(df_Open$SptDensity, points.dist.inv)

df_stream <- filter(df, df$ENV == "Stream")
points.dist <- as.matrix(dist(cbind(df_stream$Y, df_stream$X)))
points.dist.inv <- (1/points.dist)
diag(points.dist.inv) <- 0

Moran.I(df_stream$BackgroundNoise, points.dist.inv)

Moran.I(df_stream$Snr, points.dist.inv)

Moran.I(df_stream$Activity, points.dist.inv)

Moran.I(df_stream$EventsPerSecond, points.dist.inv)

Moran.I(df_stream$LowFreqCover, points.dist.inv)

Moran.I(df_stream$MidFreqCover, points.dist.inv)

Moran.I(df_stream$HighFreqCover, points.dist.inv)

Moran.I(df_stream$TemporalEntropy, points.dist.inv)

Moran.I(df_stream$EntropyOfAverageSpectrum, points.dist.inv)

Moran.I(df_stream$EntropyOfPeaksSpectrum, points.dist.inv)

Moran.I(df_stream$EntropyOfVarianceSpectrum, points.dist.inv)

Moran.I(df_stream$AcousticComplexity, points.dist.inv)

Moran.I(df_Open$ClusterCount, points.dist.inv)

Moran.I(df_Open$SptDensity, points.dist.inv)
