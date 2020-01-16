#Cluster analysis using Bayesian nonparametric clustering algorithm#
#Marina Scarpelli#
#09.01.2019#

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"), row.names = 1)

library(DIRECT)

nGene = nrow(df)
c.curr <- rep(1, nGene)    # start with a single cluster
alpha.curr <- 0.01

cluster_result <- DIRECT(df[8:14], nTime = 12, c.curr = c.curr)

crp_graph_2d(as.matrix(df[8:14]), cluster_result)
