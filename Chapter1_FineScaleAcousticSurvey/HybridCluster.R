# Title:  Hybrid Clustering - plos one paper
# Author: Yvonne Phillips
# Modified: Marina D. A. Scarpelli
# Date:  08.11.2019

# Phillips, Y.F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Note this code should only be run after 1 Normalisation and Correlation Matrix
# as it requires the results generated by this code

# Description:  This code applies a Hybrid method to cluster a large acoustic
#   dataset.  The hybrid method involves three steps:
#   1. Partition the dataset using k-means into a large number of clusters
#   2. Apply hierarchical clustering to centroids from step 1 to reduce the
#      number of clusters to 5 to 100 in steps of 5.
#   3. Assign all observations to the nearest centroid using knn (k-nearest-
#      neighbour).

# Package requirements: class
# Note this package will be downloaded if it has not been 
# previously installed

# Time requirements: about 10 hours for each k1 cluster run
# Note: this code is set up to run only the k1 = 25000 cluster run

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load the SUMMARY indices ---------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())
start_time <- paste(Sys.time())
start_time

# load normalised summary indices 
load(file="C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/normalised_indices.RData")

#I don't have missing minutes so I don't need to "complete DF"
#indices_norm_summary <- complete_DF

colnames(indices_norm)
length(indices_norm[,1])

colnames <- c("BGN", "HFC",
              "ACI", "EAS", "EPS",
              "CLC", "NDSI")
colnames(indices_norm) <- colnames
# remove the missing minutes and track the original minutes
# a is the missing minutes and z is the recorded minutes
#a <- which(is.na(indices_norm_summary$BGN))
#length(a) # length 5093 (773 + 4320 (3 days) minutes)
# save a list of integers of the recorded minutes
#z <- setdiff(1:nrow(indices_norm_summary), a)

# remove the missing minutes and the three days in October 2015
# this dataset should have 1141147 rows (nrows(indices_norm_summary))
# these will be replaced later
#indices_norm_summary <- indices_norm_summary[z,]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Hybrid Method -------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Step 1 Partitioning using kmeans

#set-up the variables
k1 <- i <- 25000   # other k1 values include 17500, 20000 and 22500
k2 <- seq(5, 100, 5)

paste(Sys.time(), " Starting kmeans clustering", sep = " ")
for (k in 1:1) {
  # The kmeans clustering takes between 1.5 and 1.75 hours for each 
  # k1 value so I have set this to only complete k1=25000
  # to run each of the k1 values change line 52 to for(k in 1:4) {
  set.seed(123)
  kmeansObj <- kmeans(indices_norm, centers = k1[k], iter.max = 100)
  print(paste(Sys.time()))
  list <- c("clusters","centers","totss","withinss","totwithinss",
            "betweenss", "size", "iter", "ifault")
  for(i in 1:length(list)){
    list[i] <- paste(list[i],k1[k],sep = "")
  }
  print(paste(Sys.time()))
  for (i in 1:length(list)) {
    assign(paste(list[i],sep = ""), kmeansObj[i])
    save(list = list[i], file = paste("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/KMeans", list[i],".Rdata",sep=""))
  }
  print(paste(Sys.time()))
  for (i in 1:length(list)) {
    rm(list = list[i]) 
  }
  print(paste(Sys.time()))
}
paste(Sys.time(), " End kmeans clustering", sep = " ")

################################################
# Step 2:  Hierarchical clustering of centers
################################################
paste(Sys.time(), "Starting hclust")
folder <- "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Results"

kmeansCenters <- kmeansObj$centers

# Hierarchically cluster the centers from kmeans
paste(Sys.time(), "Starting hclust function")
hybrid.fit.ward <- hclust(dist(kmeansCenters), method = "ward.D2")

paste(Sys.time(), "Starting cutree function")

plot(hybrid.fit.ward)

# Note: The cutree function takes about 7 hours 
# Every 12-35 minutes a file is saved in the results folder containing the 
# cluster result for the k2 value and the cluster centroids. The cluster
# centroids these remain acorss each k1 run
clusters <- NULL
for (j in k2) {
  # cut the dendrogram into k2 clusters
  hybrid.clusters <- cutree(hybrid.fit.ward, k=j)
  # generate the test dataset
  hybrid.dataset <- cbind(hybrid.clusters, kmeansCenters)
  hybrid.dataset <- as.data.frame(hybrid.dataset)
  write.csv(hybrid.dataset, 
            paste("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Results/hybrid_dataset_centers_", 
                  k1[k], "_", j, ".csv",sep=""), row.names = FALSE)
  train <- hybrid.dataset
  test <- indices_norm
  # set up class labels
  cl <- factor(unname(hybrid.clusters))
  # set the k value for the knn function 
  k3 <- sqrt(floor(nrow(train)))
  # if k3 is even reduce by one to make odd
  is.even <- function(x) x %% 2 == 0
  if(is.even(k3)=="TRUE") {
    k3 <- k3 - 1
  }
  print(Sys.time())
  # Step 3:  Use knn to assign minutes to clusters ------
  
  # The class package is required for the knn function
  #Venables, W. N. & Ripley, B. D. (2002) Modern Applied
  #Statistics with S. Fourth Edition. Springer, New York. ISBN
  #0-387-95457-0
  packages <- c("class")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  library(class)
  clusts <- knn(train[,-1], test, cl, k = k3, prob = F)
  clusters <- cbind(clusters, clusts)
  print(Sys.time())
}
paste(Sys.time(), "Ending the cutree function")

colnames(clusters) <- c("clust5", "clust10","clust15","clust20",
                        "clust25","clust30","clust35","clust40",
                        "clust45","clust50","clust55","clust60",
                        "clust65","clust70","clust75","clust80",
                        "clust85","clust90","clust95","clust100")

paste(Sys.time(), " Finishing hclust clustering", sep = " ")

# add the missing minutes back into the dataset
#full_cluster_DF <- matrix(NA, nrow = (398*1440*2), ncol=20)
#full_cluster_DF <- data.frame(full_cluster_DF)
full_cluster_DF <- clusters

colnames <- c("clust5", "clust10", "clust15", "clust20",
              "clust25", "clust30", "clust35", "clust40",
              "clust45", "clust50", "clust55", "clust60",
              "clust65", "clust70", "clust75", "clust80",
              "clust85", "clust90", "clust95", "clust100")

colnames(full_cluster_DF) <- colnames

# assign to "hclust_clusters_k1" dataframe and save as a .RData file 
assign(paste("hclust_clusters_", k1[k], sep = ""), clusters)
save(list = paste("hclust_clusters_", k1[k], sep = ""), 
     file = paste("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Results/hclust_clusters_", 
                  k1[k],".Rdata",sep=""))

# save as .csv file
write.csv(clusters, 
          paste("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Oct2019/SummaryIndices_Channel1_Prepared/Results/cluster_full_list_", 
                k1, ".csv", sep = ""), row.names=F)
as.hclust(clusters)

# To view clusters
View(clusters)
end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime
