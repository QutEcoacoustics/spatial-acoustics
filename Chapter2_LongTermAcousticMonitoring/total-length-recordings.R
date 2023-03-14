library(tidyverse)

files <- list.files(path = "D:/AIndices/1_IndicesToTs/64/253", pattern = ".csv", full.names = T)

files <- as.list(files)
df <- lapply(files, read.csv)
df <- do.call(rbind, df)

df$date <- as.factor(df$date)

df %>% nrow(.)/n_distinct(df$date)

df %>% nrow(.)/60

