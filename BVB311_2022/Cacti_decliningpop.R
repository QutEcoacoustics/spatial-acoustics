library(ggplot2)
library(tidyverse)

dataset <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/BVB311/diagnosing decline workshop data (1).csv")

t.test(dataset$Small88, dataset$Small98, paired = T, conf.level = 0.95)
t.test(dataset$Medium88, dataset$Medium98, paired = T, conf.level = 0.95)
t.test(dataset$Large88, dataset$Large98, paired = T, conf.level = 0.95)
t.test(dataset$Cattle88, dataset$Cattle98, paired = T, conf.level = 0.95)
t.test(dataset$Peccary88, dataset$Peccary98, paired = T, conf.level = 0.95)

dataset$SmallChange <- ((dataset$Small98-dataset$Small88)/dataset$Small88)*100
dataset$MediumChange <- ((dataset$Medium98-dataset$Medium88)/dataset$Medium88)*100
dataset$LargeChange <- ((dataset$Large98-dataset$Large88)/dataset$Large88)*100
dataset$PeccaryChange <- ((dataset$Peccary98-dataset$Peccary88)/dataset$Peccary88)*100
dataset$CattleChange <- ((dataset$Cattle98-dataset$Cattle88)/dataset$Cattle88)*100

summary(dataset)

smallvscattle <- lm(SmallChange ~ CattleChange, data = dataset)
summary(smallvscattle)
mediumvscattle <- lm(MediumChange ~ CattleChange, data = dataset)
summary(mediumvscattle)
largevscattle <- lm(LargeChange ~ CattleChange, data = dataset)
summary(largevscattle)
smallvspeccary <- lm(SmallChange ~ PeccaryChange, data = dataset)
summary(smallvspeccary)
mediumvspeccary <- lm(MediumChange ~ PeccaryChange, data = dataset)
summary(mediumvspeccary)
largevspeccary <- lm(LargeChange ~ PeccaryChange, data = dataset)
summary(largevspeccary)

new_dataset <- pivot_longer(dataset, cols = c(13:15), names_to = "size_class", values_to = "cacti_change") %>% 
  select(13:16)
summary(new_dataset)

ggplot(new_dataset, aes(x = cacti_change, y = CattleChange)) +
  geom_point(aes(colour = size_class))

ggplot(new_dataset, aes(x = cacti_change, y = PeccaryChange)) +
  geom_point(aes(colour = size_class))
