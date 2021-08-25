library(tidyverse)


files_path <- "Y:/Phil/ds_workshop/dataset_files/concat_and_mapped"

zscores <- function (value) {
  df_num <- as.numeric(as.matrix(value[,7:262]))
  mean <- mean(df_num)
  sd <- sd(df_num)
  zscores <- (value[,7:262]-mean)/sd
}

#Reading the dataframes containing indices
ACI <- read.csv(paste(files_path, "/ACI_all.csv", sep = ""))
ENT <- read.csv(paste(files_path, "/ENT_all.csv", sep = ""))
EVN <- read.csv(paste(files_path, "/EVN_all.csv", sep = ""))

#zscoring the DF's
znorm_ACI <- zscores(ACI)
znorm_ENT <- zscores(ENT)
znorm_EVN <- zscores(EVN)


#Binding DF's columns

complete_df <- cbind(znorm_ACI, znorm_ENT, znorm_EVN)

#Hierarchical clustering

distance_matrix <- dist(complete_df, method = "euclidean")

hcluster <- hclust(distance_matrix)
members <- cutree(hcluster, k = 15)

clustered_df <- cbind(members, ACI[,1:6])

