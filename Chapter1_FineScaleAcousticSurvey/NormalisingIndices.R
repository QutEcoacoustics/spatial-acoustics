# Title: Normalisation and Correlation Matrix - Plos One paper
# Author: Yvonne Phillips
# Version: Marina Scarpelli
# Date: 7 November 2019

# This code reads the summary indices and normalises and scales them.
# Then a correlation matrix is calculated and this is saved in a results folder

# File and folder requirements (1 data file and 1 folder): 
# These are all automatically loaded below

# Time requirements: less than 1 mintue

# Packages: NILL

##############################################
# Read Summary Indices
##############################################
# remove all objects in global environment
rm(list = ls())
start_time <- paste(Sys.time())


library(tidyverse)


# Load and read summary indices (if necessary)

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/ResultsIndices_Channel2/")

files <- list.files(directory, pattern = ".Indices.csv", full.names = T, recursive = T)

name <- basename(files)
name <- gsub(pattern = "__Towsey.Acoustic.Indices.csv", replacement = "", x = name)

files <- as.list(files)
df <- lapply(files, read.csv) 
df<-do.call(rbind, df)
df <- select(df, BackgroundNoise, Snr, Activity, EventsPerSecond, HighFreqCover, MidFreqCover, LowFreqCover, AcousticComplexity, TemporalEntropy, EntropyOfAverageSpectrum, EntropyOfPeaksSpectrum, EntropyOfVarianceSpectrum, ClusterCount, Ndsi, SptDensity)
write.csv(df, "indices_all.csv")

directory <- setwd("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared")

indices_all <- read.csv("indices_all.csv", row.names = 1) %>%  
  select(., -FileName)

######### Normalise data #################################
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
for (i in 1:ncol(indices_all)) {
  q1 <- unname(quantile(indices_all[,i], probs = 0.015, na.rm = TRUE))
  q2 <- unname(quantile(indices_all[,i], probs = 0.985, na.rm = TRUE))
  q1.values <- c(q1.values, q1)
  q2.values <- c(q2.values, q2)
  indices_norm[,i]  <- normalise(indices_all[,i], q1, q2)
}
rm(q1, q2, i)

# adjust values greater than 1 or less than 0 to 1 and 0 respectively
for (j in 1:ncol(indices_norm)) {
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
# mpg
ggqqplot(indices_all$BackgroundNoise, ylab = "BGN")
ggqqplot(indices_all$Snr, ylab = "SNR")
ggqqplot(indices_all$Activity, ylab = "ACT")
ggqqplot(indices_all$EventsPerSecond, ylab = "EVN")
ggqqplot(indices_all$HighFreqCover, ylab = "HFC")
ggqqplot(indices_all$MidFreqCover, ylab = "MFC")
ggqqplot(indices_all$LowFreqCover, ylab = "LFC")
ggqqplot(indices_all$AcousticComplexity, ylab = "ACI")
ggqqplot(indices_all$TemporalEntropy, ylab = "ENT")
ggqqplot(indices_all$EntropyOfAverageSpectrum, ylab = "EAS")
ggqqplot(indices_all$EntropyOfPeaksSpectrum, ylab = "EPS")
ggqqplot(indices_all$EntropyOfVarianceSpectrum, ylab = "ECV")
ggqqplot(indices_all$ClusterCount, ylab = "CLC")
ggqqplot(indices_all$SptDensity, ylab = "SPD")

#Data is not normally distributed, so I used spearman rank correlation#

cor <- abs(cor(indices_all, use = "complete.obs", method = "spearman"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Correlation_matrix_norm.csv")
rm(cor)


##############################################
# remove highly correlated indices - above 0.7
remove <- c(2, 9, 15)
indices_all1 <- indices_all[,-remove]
rm(remove)

cor <- abs(cor(indices_all1, use = "complete.obs", method = "spearman"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Correlation_matrix_norm1.csv")
rm(cor)

##############################################
# repeat the process to check if there are still highly correlated indices. Since there are, remove the ones highly correlated and test it again until no correlation is found
remove <- c(2, 5, 6)
indices_all2 <- indices_all1[,-remove]
rm(remove)

cor <- abs(cor(indices_all2, use = "complete.obs", method = "spearman"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Correlation_matrix_norm2.csv")
rm(cor)

# repeat the process to check if there are still highly correlated indices. Since there are, remove the ones highly correlated and test it again until no correlation is found
remove <- c(2, 7)
indices_all3 <- indices_all2[,-remove]
rm(remove)

cor <- abs(cor(indices_all3, use = "complete.obs", method = "spearman"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Correlation_matrix_norm3.csv")
rm(cor)

#Now all values were under .7 so we can move on with the analysis.

indices_all <- indices_all3

rm(cor, indices_all1, indices_all2, indices_all3)

# IMPORTANT:  These are used to name the plots
site <- c("WA", "WB")
index <- "SELECTED_Final" # or "ALL"
type <- "Summary"
paste("The dataset contains the following indices:"); colnames(indices_all)

# Generate a list of the missing minutes in summary indices

missing_minutes_summary <- which(is.na(indices_all[,1]))

#No missing minutes so no need to save

length(missing_minutes_summary)

###########################################################
# Create a normalised dataset between 1.5 and 98.5% bounds 
###########################################################
indices_norm <- indices_all

# normalise values between 1.5 and 98.5 percentile bounds
q1.values <- NULL
q2.values <- NULL
for (i in 1:ncol(indices_all)) {
  q1 <- unname(quantile(indices_all[,i], probs = 0.015, na.rm = TRUE))
  q2 <- unname(quantile(indices_all[,i], probs = 0.985, na.rm = TRUE))
  q1.values <- c(q1.values, q1)
  q2.values <- c(q2.values, q2)
  indices_norm[,i]  <- normalise(indices_all[,i], q1, q2)
}
rm(q1, q2, i)

# adjust values greater than 1 or less than 0 to 1 and 0 respectively
for (j in 1:ncol(indices_norm)) {
  a <- which(indices_norm[,j] > 1)
  indices_norm[a,j] = 1
  a <- which(indices_norm[,j] < 0)
  indices_norm[a,j] = 0
}
#paste(indices_norm[417000,4], digits=15)
save(indices_norm, file="C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/normalised_indices.RData")
load(file="C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/normalised_indices.RData")
#paste(indices_norm[417000,4], digits=15)

# replace the missing minutes
complete_DF <- matrix(NA, nrow = (398*1440*2), ncol = 12)
complete_DF <- as.data.frame(complete_DF)
complete_DF <- indices_norm
colnames <- colnames(indices_norm)
colnames(complete_DF) <- colnames

save(complete_DF, file="C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Results/normalised_summary_indices.RData", 
     row.names = F)

rm(complete_DF, indices_norm)

end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime
