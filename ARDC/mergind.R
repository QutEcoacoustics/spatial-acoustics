#Merging df
library(tidyverse)

df1 <- read.csv("Y:/Znidersic/Wetlands/2021-12-22/EcosoundsBatchFile_CreatedForAnthony.v3_tidy.csv")

df_pointID <- read.csv("Y:/Znidersic/Wetlands/2021-12-22/EcosoundsBatchFile_summary_pointID.csv")

merged <- left_join(df1, df_pointID, by = "Point.Name") %>% 
  write.csv("Y:/Znidersic/Wetlands/2021-12-22/EcosoundsBatchFile_CreatedForAnthony.v3_final.csv", row.names = F)
