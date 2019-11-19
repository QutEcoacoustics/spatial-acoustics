library(tidyverse)
library(spdep)
library(vegan)
library(adegenet)
library(ape)

df <- read.csv("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/Pontos_ind_metricas.csv", header = TRUE, row.names = 29)
points.dist <- as.matrix(dist(cbind(df$Y, df$X)))
points.dist.inv <- 1/points.dist
diag(points.dist.inv) <- 0

Moran.I(df$BackgroundNoise, points.dist.inv)

Moran.I(df$Snr, points.dist.inv)

Moran.I(df$Activity, points.dist.inv)

Moran.I(df$EventsPerSecond, points.dist.inv)

Moran.I(df$LowFreqCover, points.dist.inv)

Moran.I(df$MidFreqCover, points.dist.inv)

Moran.I(df$HighFreqCover, points.dist.inv)

Moran.I(df$TemporalEntropy, points.dist.inv)

Moran.I(df$EntropyOfAverageSpectrum, points.dist.inv)

Moran.I(df$EntropyOfPeaksSpectrum, points.dist.inv)

Moran.I(df$EntropyOfVarianceSpectrum, points.dist.inv)

Moran.I(df$AcousticComplexity, points.dist.inv)

Moran.I(df$ClusterCount, points.dist.inv)

Moran.I(df$SptDensity, points.dist.inv)

df.matrix <- select(df, 6:26)
XY <- select(df, 33:34)
  

spca(df.matrix, xy = XY)

plot(df$X, df$Y)

abline(h=c(2, 4, 6, 8), lty=2) #as linhas na horizontal
abline(v=c(0.5, 1, 1.5, 2), lty=2) # linhas verticais

dist.esp <- vegdist(c(df$X, df$Y), method = "euclid")
indices <- 