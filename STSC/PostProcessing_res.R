#TS Graphs - Checking for overlaps and true positives

library(tidyverse)
library(ggplot2)

ts <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/WB25/18.08.2020_WB25_OCT.csv") %>% 
  mutate(point = "WB25")
  rename(., position = X) %>% 
  select(., position, AcousticComplexity)

res <- read.table("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/WB15/resBGN_wb15._432.txt")

obs_num <- as.numeric(count(res))

res <- res %>% rename(FirstInstance_Start = V1,
         FirstInstance_End = V2,
         SecondInstance_Start = V3,
         SecondInstance_End = V4,
         Lenght = V5,
         Distance = V6) %>% 
  mutate(., id = 1:obs_num) %>% 
  select(., id, everything()) %>% 
  filter(., Distance <= "2.7") %>% 
  pivot_longer(., cols = 2:5, names_to = "Instance", values_to = "position") %>% 
  mutate(., Instance = gsub(pattern = "FirstInstance", replacement = "motif", x = Instance)) %>%
  mutate(., Instance = gsub(pattern = "SecondInstance", replacement = "match", x = Instance)) %>%
  separate(., Instance, into = c("instance", "moment"), sep = "_") %>% 
  mutate(., instance = paste(id, instance, sep = "_")) %>% 
  pivot_wider(., names_from = moment, values_from = position)

ggplot(res, aes(x = Lenght, y = Distance)) +
      geom_smooth()
  
df <- full_join(ts, res, by = c("position" = "Start"), keep = T) #%>% #After doing that the motif id (instance) tells when the motif/match starts and the column end tells you where it ends. It duplicates positions if there is more than one motif starting at the same point#

  #select(., -"NA") #%>% 
  #mutate(., instance_id = case_when(Start >= fid  | End <= fid ~ instance,
                                    #Start != fid | End != fid ~ "error"))

  #write.csv(., "C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/WB06/ACI/20.08.2020_WB06_resACI.csv")  


test <- read.csv("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/WB06/ACI/18.08.2020_WB06_OCT_ACI_test1.csv")

test %>% filter(num == "4") %>% 
  ggplot(., aes(x = overlap_position, y = value)) +
  geom_line(aes(linetype = as.factor(what))) +
  #scale_colour_manual(values = c("#b2182b", "#2166ac", "#4d4d4d")) +
  scale_linetype_manual(values=c("dotted", "solid")) +
  theme_classic() +
  annotate("text", x = 5, y = -0.3, label = "Number: 116") +
  annotate("text", x = 5, y = -0.33, label = "Distance: 9.34") +
  theme(legend.title = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  #guides(linetype = FALSE) +
  ggsave("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/Project/STSC/WB06/ACI/Figures/21.08.2020_Motif4.jpg")
    #+
    #facet_wrap(~num)




