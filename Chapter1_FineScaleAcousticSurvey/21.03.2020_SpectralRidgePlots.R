library(tidyverse)
library(ggridges)
library(ggplot2)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

ACI_Spectral <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "ResultsIndices_Channel1", "JZ001_WB28" ,"BOW-JZ1-WB28_20191014_180000.wav", "Towsey.Acoustic", "BOW-JZ1-WB28_20191014_180000__Towsey.Acoustic.ACI.csv"))

t <- t(ACI_Spectral)

d <- ggplot(ACI_Spectral, aes(x = 2, y = 3:259))

g <- d + geom_bar()
