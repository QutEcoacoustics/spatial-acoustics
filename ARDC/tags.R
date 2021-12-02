library(tidyverse)
library(taxize)

a2o <- read.csv("C:/Users/scarp/Queensland University of Technology/Open Ecoacoustics - Documents/Nina/a2o-tags-1636074860574_new.csv")
ecosounds <- read.csv("C:/Users/scarp/Queensland University of Technology/Open Ecoacoustics - Documents/Nina/ecosounds-tags-1636074769065_new.csv")

new_tags <- merge(a2o, ecosounds, by = "text", all = T) %>% 
  select(text, id_a2o, id_ecosounds, type_of_tag.x, type_of_tag.y, retired.x, retired.y) 


get_ids(new_tags$text)
