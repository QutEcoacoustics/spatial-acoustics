library(rucrdtw)
library(dtwclust)

query <- read.table("C:/Work/STSC/resACI_wb06_oct_432.txt")
data <- read.table("C:/Work/STSC/12.08.2020_WB06_OCT_ACI.txt")

ucrdtw_vv(data, query, dtwwindow = 32)
