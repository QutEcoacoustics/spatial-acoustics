library(tidyverse)

rm(list = ls())

####Building TS

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}


files <- list.files(getDataPath("STSC", "Results", "Bowraoct"), pattern = "res", recursive = T)


####After motif - processing .txt file

# point_id <- "WB46"
# point_id_lower <- "wb46"
# index_abb1 <- "EVN"

for (file in files) {
  read.table(getDataPath("STSC", "Results", "Bowraaug", file), sep = " ", blank.lines.skip = T, fileEncoding = "UTF-16") %>%
    select(., 2:7) %>% 
    write.table(., getDataPath("STSC", "Results", "Bowraaug", file), row.names = F, col.names = F)
}

# motifs_evn <- read.table(getDataPath("STSC", paste("res", index_abb1, "_", point_id_lower, "_432.txt", sep = "")), sep = " ", blank.lines.skip = T, fileEncoding = "UTF-16") %>% 
#   select(., 2:7)
# 
# write.table(motifs_evn, getDataPath("STSC", paste("result", index_abb1, "_", point_id, "_432.txt", sep = "")), row.names = F, col.names = F)
# 
# index_abb2 <- "ACI"
# 
# motifs_aci <- read.table(getDataPath("STSC", paste("res", index_abb2, "_", point_id_lower, "_432.txt", sep = "")), sep = " ", blank.lines.skip = T, fileEncoding = "UTF-16") %>% 
#   select(., 2:7)
# 
# write.table(motifs_aci, getDataPath("STSC", paste("result", index_abb2, "_", point_id, "_432.txt", sep = "")), row.names = F, col.names = F)
# 
# index_abb3 <- "ENT"
# 
# motifs_ent <- read.table(getDataPath("STSC", paste("res", index_abb3, "_", point_id_lower, "_432.txt", sep = "")), sep = " ", blank.lines.skip = T, fileEncoding = "UTF-16") %>% 
#   select(., 2:7)
# 
# write.table(motifs_ent, getDataPath("STSC", paste("result", index_abb3, "_", point_id, "_432.txt", sep = "")), row.names = F, col.names = F)


