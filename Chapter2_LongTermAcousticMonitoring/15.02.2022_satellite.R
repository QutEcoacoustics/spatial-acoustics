rm(list = ls())
library(tidyverse)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment", ...))
}

satellite <- read.csv(getDataPath("14.02.2022_RemoteSensingIndices.csv"))

cor.test(satellite$EBI_MEAN, satellite$EBI_RANGE)
cor.test(satellite$EBI_MEAN, satellite$GNDVI_MEAN)
cor.test(satellite$EBI_MEAN, satellite$GNDVI_RANGE)
cor.test(satellite$EBI_MEAN, satellite$NDVI_MEAN)
cor.test(satellite$EBI_MEAN, satellite$NDVI_RANGE)
cor.test(satellite$EBI_MEAN, satellite$MSAVI_MEAN)
cor.test(satellite$EBI_MEAN, satellite$MSAVI_RANGE)
cor.test(satellite$EBI_RANGE, satellite$GNDVI_MEAN)
cor.test(satellite$EBI_RANGE, satellite$GNDVI_RANGE)
cor.test(satellite$EBI_RANGE, satellite$NDVI_MEAN)
cor.test(satellite$EBI_RANGE, satellite$NDVI_RANGE)
cor.test(satellite$EBI_RANGE, satellite$MSAVI_MEAN)
cor.test(satellite$EBI_RANGE, satellite$MSAVI_RANGE)
cor.test(satellite$GNDVI_MEAN, satellite$GNDVI_RANGE)
cor.test(satellite$GNDVI_MEAN, satellite$NDVI_MEAN)
cor.test(satellite$GNDVI_MEAN, satellite$NDVI_RANGE)
cor.test(satellite$GNDVI_MEAN, satellite$MSAVI_MEAN)
cor.test(satellite$GNDVI_MEAN, satellite$MSAVI_RANGE)
cor.test(satellite$GNDVI_RANGE, satellite$NDVI_MEAN)
cor.test(satellite$GNDVI_RANGE, satellite$NDVI_RANGE)
cor.test(satellite$GNDVI_RANGE, satellite$MSAVI_MEAN)
cor.test(satellite$GNDVI_RANGE, satellite$MSAVI_RANGE)
cor.test(satellite$NDVI_MEAN, satellite$NDVI_RANGE)
cor.test(satellite$NDVI_MEAN, satellite$MSAVI_MEAN)
cor.test(satellite$NDVI_MEAN, satellite$MSAVI_RANGE)
cor.test(satellite$NDVI_RANGE, satellite$MSAVI_MEAN)
cor.test(satellite$NDVI_RANGE, satellite$MSAVI_RANGE)
cor.test(satellite$MSAVI_MEAN, satellite$MSAVI_RANGE)


cor(satellite[,2:4])
cor(satellite[,3:4])

sat <- satellite %>% select(., Date, EBI_RANGE, NDVI_MEAN, MSAVI_RANGE)

df <- read.csv(getDataPath("08.02.2022_completedf.csv")) %>% 
  select(everything(), -X)

ggplot(satellite, aes(x = Date, y = EBI_MEAN)) +
         geom_smooth(aes(x = Date))


merged_df <- merge(sat, df, by.x = c("Date"), by.y = c("Date"), all.y = T)

write.csv(merged_df, getDataPath("15.02.2022_completedf.csv"))
