rm(list = ls())
library(tidyverse)
library(ggplot2)

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter2_SoundscapeTemporalAssessment", ...))
}

data <- read.csv(getDataPath("15.02.2022_completedf.csv"))

data$month <- as.factor(data$month)

#Colours in alphabetical order
#e31a1c anthrophony
#fdbf6f anthrophony/biophony
#fb9a99 anthrophony/biophony/geophony 
#ff7f00 anthrophony/geophony
#33a02c biophony
#b2df8a biophony/geophony
#1f78b4 geophony

# 
# 
# base_plot <- ggplot(data, aes(x = month)) + 
#   geom_bar(aes(fill = general_category)) +
#   scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4"))
# 
# base_plot + geom_boxplot(aes(y = TempOut * 10)) +
#   scale_y_continuous("Count",
#                      sec.axis = sec_axis(~ . /10, name = "Approx Temperature"))
# 
# df_biophony <- filter(df, general_category == "biophony" | general_category == "anthrophony/biophony" | general_category == "anthrophony/biophony/geophony" | general_category == "biophony/geophony") %>% 
#   droplevels(.)
# 
# df_biophony$month <- as.factor(df_biophony$month)
# 
# bio_plot <- ggplot(df_biophony, aes(x = month)) + 
#   geom_bar(aes(fill = general_category)) +
#   scale_fill_manual(values = c("#fdbf6f", "#fb9a99", "#33a02c", "#b2df8a", "#1f78b4"))
# 
# bio_plot + geom_boxplot(aes(y = TempOut * 10)) +
#   scale_y_continuous("Count",
#                      sec.axis = sec_axis(~ . /10, name = "Approx Temperature"))
# summary(data)

#Putting data into long format ----

data_long <- pivot_longer(data, cols = c(3:5,36), names_to = "space_vars", values_to = "space_vars_value") %>% 
  select(., Date, week_day, Time, Recording_time, month, space_vars, space_vars_value, RFclass, general_category, everything())

data_long <- pivot_longer(data_long, cols =c(11:13), names_to = "climatic_vars", values_to = "climatic_vars_value")%>% 
  select(., Date, week_day, Time, Recording_time, month, space_vars, space_vars_value, climatic_vars, climatic_vars_value, RFclass, general_category, everything())



#Monthly plots ----

#Space vars ----

data_long %>% filter(space_vars != "moon_illu") %>% 
  ggplot(., aes(x = month)) + 
    geom_bar(aes(fill = general_category)) +
    scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
    geom_boxplot(aes(y = space_vars_value*1000)) +
  #scale_fill_manual(values = c("#0047bb", "#4dff4d", "#ff8533"))
  #scale_y_continuous("Count",
                       #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
    facet_wrap(~ space_vars)

data_long %>% filter(space_vars != "moon_illu" & general_category == "biophony") %>% 
  ggplot(., aes(x = month)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#33a02c")) +
  geom_boxplot(aes(y = space_vars_value*1000)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ space_vars)

data_long %>% filter(space_vars != "moon_illu" & general_category == "biophony" & RFclass != "froginsect") %>% 
  ggplot(., aes(x = month)) + 
  geom_bar(aes(fill = RFclass)) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  geom_boxplot(aes(y = space_vars_value*1000, colour = space_vars)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ RFclass)

#Climatic vars----

data_long %>% 
  ggplot(., aes(x = month)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
  geom_boxplot(aes(y = climatic_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ climatic_vars)

data_long %>% filter(space_vars != "moon_illu" & general_category == "biophony") %>% 
  ggplot(., aes(x = month)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#33a02c")) +
  geom_boxplot(aes(y = climatic_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ climatic_vars)


data_long %>% filter(space_vars != "moon_illu" & general_category == "biophony" & RFclass != "froginsect") %>% 
  ggplot(., aes(x = month)) + 
  geom_bar(aes(fill = RFclass)) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  geom_boxplot(aes(y = climatic_vars_value*10, colour = climatic_vars)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ RFclass)


#bird #c51b7d
#birdinsect #e9a3c9
#insect #5ab4ac
#froginsect #4d9221

#Daily plots ----
#Space vars ----

data_long %>% #filter(space_vars == "moon_illu") %>% 
  ggplot(., aes(x = Date)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
  geom_jitter(aes(y = space_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ space_vars)

data_long %>% filter(general_category == "biophony") %>% 
  ggplot(., aes(x = Date)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#33a02c")) +
  geom_boxplot(aes(y = space_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ space_vars)

data_long %>% filter(space_vars == "moon_illu" & general_category == "biophony" & RFclass != "froginsect") %>% 
  ggplot(., aes(x = Date)) + 
  geom_bar(aes(fill = RFclass)) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  geom_jitter(aes(y = space_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ RFclass)

#Climatic vars ----

data_long %>% 
  ggplot(., aes(x = Date)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
  geom_boxplot(aes(y = climatic_vars_value)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ climatic_vars)

data_long %>% filter(general_category == "biophony") %>% 
  ggplot(., aes(x = Date)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#33a02c")) +
  geom_boxplot(aes(y = climatic_vars_value)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ climatic_vars)

data_long %>% filter(general_category == "biophony" & RFclass != "froginsect") %>% 
  ggplot(., aes(x = Date)) + 
  geom_bar(aes(fill = RFclass)) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  geom_boxplot(aes(y = climatic_vars_value, colour = climatic_vars)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ RFclass)
  
#Hourly plots ----
#Climatic vars ----

data_long$Recording_time <- factor(data_long$Recording_time, levels = c("0:00:00", "2:00:00", "4:00:00", "6:00:00", "8:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00"))

data_long <- separate(data_long, col = "Recording", into = c("hour", "minute", "second"), remove = F, sep = ":")

data_long %>% 
  ggplot(., aes(x = Recording_time)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#e31a1c", "#fdbf6f", "#fb9a99", "#ff7f00", "#33a02c", "#b2df8a", "#1f78b4")) +
  geom_boxplot(aes(y = climatic_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ climatic_vars)

data_long %>% filter(general_category == "biophony") %>% 
  ggplot(., aes(x = Recording_time)) + 
  geom_bar(aes(fill = general_category)) +
  scale_fill_manual(values = c("#33a02c")) +
  geom_boxplot(aes(y = climatic_vars_value*10)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ climatic_vars)

data_long %>% filter(general_category == "biophony" & RFclass != "froginsect") %>% 
  ggplot(., aes(x = Recording_time)) + 
  geom_bar(aes(fill = RFclass)) +
  scale_fill_manual(values = c("#c51b7d", "#e9a3c9", "#4d9221", "#5ab4ac")) +
  geom_boxplot(aes(y = climatic_vars_value*10, colour = climatic_vars)) +
  #scale_y_continuous("Count",
  #sec.axis = sec_axis(~ . /10, name = "Approx value")) +
  facet_wrap(~ RFclass)




ggplot(data, aes(x = week_day, fill = general_category)) + 
  geom_bar()

plot(data$month, data$TempOut)
ggplot(data, aes(x = month, y = TempOut)) +
  geom_jitter()

plot(data$month, data$HumOut)
ggplot(data, aes(x = month, y = HumOut)) +
  geom_jitter()

plot(data$month, data$Rain)
ggplot(data, aes(x = month, y = Rain)) +
  geom_jitter()

plot(data$month, data$EBI_RANGE)
ggplot(data, aes(x = month, y = EBI_RANGE)) +
  geom_jitter()

plot(data$month, data$NDVI_MEAN)
plot(data$month, data$MSAVI_RANGE)

ggplot(data, aes(x = month, fill = RFclass)) + 
  geom_bar() +
  

ggplot(data, aes(x = week_day, fill = RFclass)) + 
  geom_bar()



