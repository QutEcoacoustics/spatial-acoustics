library(dtwclust)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra",  ...))
}

getDataPath <- function (...) {
  return(file.path("C:/Users/Nina Scarpelli/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra",  ...))
}

df_withrownames <- read.csv(getDataPath("Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation.csv"), row.names = 3)

df <- read.csv(getDataPath("Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation.csv"))

timeseries <- tslist(df[11:17])

tscluster_fuzzy <- tsclust(timeseries, type = "fuzzy", k = 5, seed = 123)
plot(tscluster_fuzzy)

tscluster_hierarchical <- tsclust(timeseries, type = "hierarchical", k = 5, seed = 123)
plot(tscluster_hierarchical)

library(pdc)

pdclust()

timeseries_pca <- tslist(df_pc[91:93])

tscluster_fuzzy <- tsclust(timeseries_pca, type = "fuzzy", k = 5, seed = 123)
plot(tscluster_fuzzy)

tscluster_hierarchical <- tsclust(timeseries_pca, type = "hierarchical", k = 5, seed = 123)
plot(tscluster_hierarchical)



