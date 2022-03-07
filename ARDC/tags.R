library(tidyverse)


a2o <- read.csv("C:/Users/n10393021/Queensland University of Technology/Open Ecoacoustics - Documents/Nina/a2o-tags-1636074860574_new.csv")
ecosounds <- read.csv("C:/Users/n10393021/Queensland University of Technology/Open Ecoacoustics - Documents/Nina/ecosounds-tags-1636074769065_new.csv")

new_tags <- read.csv("C:/Users/n10393021/Queensland University of Technology/Open Ecoacoustics - Documents/Nina/all_tags.csv")

new_tags_obs <- merge(new_tags, ecosounds, by = "id_ecosounds", all = T) %>% 
  select(., text.x, text.y, id_a2o, id_ecosounds, type_of_tag.x, type_of_tag.y, retired.x, retired.y, lsid.x, lsid.y, obs.x, obs.y) %>% 
  write.csv("C:/Users/n10393021/Queensland University of Technology/Open Ecoacoustics - Documents/Nina/all_tags2.csv", row.names = F)
