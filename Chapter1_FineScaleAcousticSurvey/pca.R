#PCA

library(vegan)
 g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_ACI.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$Transectpoint)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u[,1:51]

# adjust values greater than 1 or less than 0 to 1 and 0 respectively
for (j in 1:ncol(pc)) {
  a <- which(pc[,j] > 1)
  pc[a,j] = 1
  a <- which(pc[,j] < 0)
  pc[a,j] = 0
}
##############################################
# Correlation matrix (Summary Indices) of thirteen months (398 days) at two sites
##############################################
cor <- abs(cor(pc, use = "complete.obs"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/NewResults/Correlation_matrix_norm.csv")
rm(cor)

##############################################
# Normalise the selected spectral indices
#############################################
# The code that follows shows how the normalised spectral indices file
# were saved
# remove highly correlated indices
remove <- c(9,14)
pc <- pc[,-remove]
rm(remove)

# IMPORTANT:  These are used to name the plots
site <- c("Bowra")
index <- "ACI" # or "ALL"
type <- "Spectral"
paste("The dataset contains the following indices:"); colnames(pc)


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
  kmeansObj <- kmeans(pc, centers = k1[k], iter.max = 100)
  print(paste(Sys.time()))
  list <- c("clusters","centers","totss","withinss","totwithinss",
            "betweenss", "size", "iter", "ifault")
  for(i in 1:length(list)){
    list[i] <- paste(list[i],k1[k],sep = "")
  }
  print(paste(Sys.time()))
  for (i in 1:length(list)) {
    assign(paste(list[i],sep = ""), kmeansObj[i])
    save(list = list[i], file = paste("C:/plos-visualization-paper/results/kmeans", list[i],".Rdata",sep=""))
  }
  print(paste(Sys.time()))
  for (i in 1:length(list)) {
    rm(list = list[i]) 
  }
  print(paste(Sys.time()))
}
paste(Sys.time(), " End kmeans clustering", sep = " ")



g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_BGN.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_CVR.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_ENT.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_EVN.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_PMN.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_RHZ.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_RNG.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_RPS.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_RVT.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

rm(list = ls())

g <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/Outputs_SpectralIndices/test_spectralindices_SPT.csv")

resultado <- rda(g[,4:259], scale = T)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = g$time)

screeplot(resultado, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
