library(tidyverse)

TI <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/PuttingSoundIntoLand/TI.csv")

AB1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/PuttingSoundIntoLand/AB1.csv")

AB2 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/PuttingSoundIntoLand/AB2.csv")

AB3 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/PuttingSoundIntoLand/AB3.csv")

AK <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/PuttingSoundIntoLand/AK.csv")

completedf <- rbind(TI, AB1, AB2, AB3, AK) %>% 
  select(., Publication.Type, Authors, Article.Title, Source.Title, Abstract, Publication.Date, Publication.Year) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/PuttingSoundIntoLand/df.csv", row.names = F)

completedf %>% count(Publication.Year) %>% 
  plot(., type = "h")
