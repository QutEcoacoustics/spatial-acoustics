library(tidyverse)
library(ggridges)
library(ggplot2)
library(plot.matrix)

#getDataPath <- function (...) {
  #return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
#}

getDataPath <- function (...) {
  return(file.path("C:/Users/Nina Scarpelli/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

for (file in files) {
  
}

ACI_Spectral <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "ResultsIndices_Channel1", "JZ001_WB28" ,"BOW-JZ1-WB28_20191014_180000.wav", "Towsey.Acoustic", "BOW-JZ1-WB28_20191014_180000__Towsey.Acoustic.ACI.csv"), row.names = 2) %>% 
  select(2:258)

object.size(ACI_Spectral)

t <- t(ACI_Spectral)


d <- plot(t, border = NA)

g <- d + geom_bar()
