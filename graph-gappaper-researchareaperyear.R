library(openxlsx)
library(tidyverse)
library(dplyr)
library(rpivotTable)
library(ggplot2)
library(tidyr)
library(stringr)



#filtering and rearranging data#
#Here I took the categories with most papers and grouped the remaining as "others"
dados <- read.xlsx("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/Masters_Project/GAP/2018.04.02data_filtered.xlsx", colNames = T)
dados <- as_tibble(dados) %>%
  mutate(year = as.integer(year),
         BRA = case_when(big_research_area == "general_ecology" |
                           big_research_area == "landscape_related"|
                           big_research_area == "mapping" | 
                           big_research_area == "marine_soundscapes" |
                           big_research_area == "planning_and_guides" | 
                           big_research_area == "sound_description"~ "others",
                         TRUE ~ big_research_area))

dados
str(dados)

#research area/number of papers#
dados %>%
  filter(BRA != "no") %>%
  group_by(year, BRA) %>%
  count() %>%
  arrange(desc(n)) %>%
  {ggplot(., aes(x = year, y = n, fill = reorder(BRA, n))) +
  geom_bar(stat = "identity", size = 2, position = "stack") +
  scale_x_continuous(breaks = seq(min(.$year), max(.$year), 2)) +
  scale_fill_manual(labels = c("Urban soundscapes", "Noise properties", "Other areas", "Zoology", "Acoustic ecology", "Human-related" ),
                    values = c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e")) +
  labs(y = "Number of published papers", x = "Year", fill = "Research area:") +
  coord_cartesian() + #quando o grafico fica normal - n e year
  #coord_flip()+ #colocar quando vc quer ela ao contrario (x e y trocados)
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom") +
    ggsave("09.09.2019_areapapers_colour2.jpg", plot = last_plot(), dpi = 300, units = "cm", height = 15.5, width = 15.5)}

#research area/number of papers - grey scale#
dados %>%
  filter(BRA != "no") %>%
  group_by(year, BRA) %>%
  count() %>%
  arrange(desc(n)) %>%
  {ggplot(., aes(x = year, y = n, fill = reorder(BRA, n))) +
      geom_bar(stat = "identity", size = 2, position = "stack") +
      scale_x_continuous(breaks = seq(min(.$year), max(.$year), 2)) +
      scale_fill_manual(labels = c("Urban soundscapes", "Noise properties", "Other areas", "Zoology", "Acoustic ecology", "Human related"),
                        values = c("#f7f7f7", "#d9d9d9", "#bdbdbd", "#969696", "#636363", "#252525")) +
      #scale_fill_manual(labels = c("Urban impacts", "Noise", "Other areas", "Zoology", "Acoustic mapping", "Human related" ),
                        #values = c("#f7f7f7", "#d9d9d9", "#bdbdbd", "#969696", "#636363", "#252525" )) +
      labs(y = "Number of published papers", x = "Year", fill = "Research area:") +
      coord_cartesian() +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = "bottom") +
      ggsave("09.09.2019_areapapers_grey2.jpg", plot = last_plot(), dpi = 600, units = "cm", height = 15.5, width = 15.5)}

####The next ones are other graphs - feel free to use it if you want, but it's a mess. If you need help decifring my horrible coding let me know####


tab <- dados%>%
  filter(BRA != "no") %>%
  count(BRA) %>%
  mutate(perc = n/sum(n)*100)


write.csv(tab, "researcharea.csv")

dados

#country/number of papers -> do as map, don't use this one#
dados %>%
  filter(COS != "no") %>%
  count(COS)  %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n/sum(n)*100, 2)) %>%
  #write.csv(., "numberofpapersPerCountry.csv")
  filter(COS != "others") %>%
  {ggplot(., aes(x = reorder(COS, desc(n)), y = n, fill=COS)) +
      geom_bar(stat = "identity", size = 2, position = "stack") +
      geom_text(aes(label=paste0(perc,"%")), vjust=-1) +
      scale_x_discrete(labels = c("USA", "China", "England", "Australia", "Brazil")) +
      scale_fill_manual(labels = c("USA", "China", "England", "Australia", "Brazil"),
                        values = c("#66c2a5", "#fc8d62", "#8da0cb", "#a6cee3", "#1f78b4")) +
      labs(y = "Number of papers", x = "Country of study", fill = "Country of study") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "bottom")
}

#Sound type/number of papers - from total#
dados %>%
  filter(ST != "no") %>%
  count(ST)  %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n/sum(n)*100, 2)) %>%
  filter(ST != "others") %>%
  {ggplot(., aes(x = reorder(ST, desc(n)), y = n, fill=ST)) +
      geom_bar(stat = "identity", size = 2, position = "stack") +
      geom_text(aes(label=paste0(perc,"%")), vjust=-0.75) +
      scale_x_discrete(labels = c("Anthrophony", "All", "Biophony", "Geophony")) +
      scale_fill_manual(labels = c("Anthrophony", "All", "Biophony", "Geophony"),
                        values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) +
      labs(y = "Number of papers", x = "Sound type", fill = "Sound type") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "bottom") +
    ggsave("soundpapers_grey1.jpg", plot = last_plot(), dpi = 600, units = "cm", height = 7.5, width = 15.5)}

#Sound type/number of papers - inside acoustic ecology and mapping#
dados %>%
  filter(ST_aem != "no", ST_aem != "other") %>%
  count(ST_aem)  %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n/sum(n)*100, 2)) %>%
  filter(ST_aem != "anthrophony_and_biophony", 
         ST_aem != "anthrophony_and_geophony", 
         ST_aem != "biophony_and_geophony") %>%
  {ggplot(., aes(x = reorder(ST_aem, desc(n)), y = n, fill=ST_aem)) +
      geom_bar(stat = "identity", size = 2, position = "stack") +
      geom_text(aes(label=paste0(perc,"%")), vjust=-0.50) +
      scale_x_discrete(labels = c("All", "Anthrophony", "Biophony", "Geophony")) +
      scale_fill_manual(labels = c("All", "Anthrophony", "Biophony", "Geophony"),
                        values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
      labs(y = "Number of papers", x = "Sound type", fill = "Sound type") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "bottom")}

View(dados)

#sound_type per category - in colour#
dados %>% 
  select(ua, ST, ST_aem, ST_hr, ST_zoo) %>% 
  gather(., key = "key", value = "value", ST, ST_aem, ST_hr, ST_zoo) %>% 
  mutate(value = str_to_title(value)) %>% 
  filter(value == "All" |
           value == "Anthrophony" |
           value == "Biophony" |
           value == "Geophony") %>% 
  group_by(key, value) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(key) %>%
  mutate(perc = round((n/sum(n))*100)) %>%
  {ggplot(., aes(x = value, y = perc, fill = key)) +
      geom_bar(stat = "identity", position = "dodge") + #para colocar as barrinhas em cima uma da outra = stack
      #coord_flip() +
      coord_cartesian() +
      scale_fill_manual(labels = c("Total", "Acoustic Ecology", "Human-related", "Zoology"),
      values = c("#fdb863",  "#5ab4ac", "#01665e", "#c7eae5")) +
      #scale_fill_brewer(labels = c("Total", "Acoustic Mapping"),
                        #palette = "Greys") +
      labs(y = "% of papers", x = "Sound type", fill = "Research Area") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "right") +
      ggsave("09.09.2019_sound_colour.png", plot = last_plot(), dpi = 300, units = "cm", height = 7.5, width = 15.5)}

#sound_type per category - in black and white#
dados %>% 
  select(ua, ST, ST_aem, ST_hr, ST_zoo) %>% 
  gather(., key = "key", value = "value", ST, ST_aem, ST_hr, ST_zoo) %>% 
  mutate(value = str_to_title(value)) %>% 
  filter(value == "All" |
           value == "Anthrophony" |
           value == "Biophony" |
           value == "Geophony") %>% 
  group_by(key, value) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(key) %>%
  mutate(perc = round((n/sum(n))*100)) %>%
  #write.csv(., "SoundTypePerCategory.csv")
  {ggplot(., aes(x = value, y = perc, fill = key)) +
      geom_bar(stat = "identity", position = "dodge") + #para colocar as barrinhas em cima uma da outra = stack
      #coord_flip() +
      coord_cartesian() +
      scale_fill_manual(labels = c("All", "Acoustic Ecology", "Human-related", "Zoology"),
                        values = c("#cccccc", "#969696", "#636363", "#252525")) +
      #scale_fill_brewer(labels = c("Total", "Acoustic Mapping"),
      #palette = "Greys") +
      labs(y = "% of papers", x = "Sound type", fill = "Research Area") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "right") +
      ggsave("09.09.2019_sound_grayscale.png", plot = last_plot(), dpi = 300, units = "cm", height = 7.5, width = 15.5)}

#second figure with n

dados %>% 
  select(ua, ST, ST_aem) %>% 
  gather(., key = "key", value = "value", ST, ST_aem) %>% 
  mutate(value = str_to_title(value)) %>% 
  filter(value == "All" |
           value == "Anthrophony" |
           value == "Biophony" |
           value == "Geophony") %>% 
  group_by(key, value) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(key) %>% 
 #mutate(Perc = round(n*100/sum(n), 2)) %>% 
  {ggplot(., aes(x = value, y = n, fill = key)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") + #para colocar as barrinhas em cima uma da outra = stack
      #coord_flip() +
      coord_cartesian() +
      scale_fill_manual(labels = c("Total", "Acoustic Mapping"),
                        values = c("#66c2a5", "#fc8d62")) +
      #scale_fill_brewer(labels = c("Total", "Acoustic Mapping"),
      #palette = "Greys") +
      labs(y = "N of papers", x = "Sound type", fill = "Research Area") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "right") +
      ggsave("sound_colour_n.png", plot = last_plot(), dpi = 300, units = "cm", height = 7.5, width = 15.5)}
  
#area_group with environment and sound_type
dados %>%
  #filter(ST_aem != "no", ST_aem != "other") %>%
  count(ag)  %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n/sum(n)*100, 2)) %>%
         {ggplot(., aes(x = ag, y = perc, fill=ag)) +
             geom_bar(stat = "identity", position = "dodge", size = 2) +
             scale_x_discrete(labels = c("All", "Lab", "Natural", "Natural (Impacted)", "No Info", 
                                         "Other", "Urban", "Urban Green Areas"))} # +
             #scale_fill_manual(labels("All", "Lab", "Natural", "Natural (impacted)", "No Information", 
                                    #  "Other", "Urban", "Urban Green Areas")) +
                              # values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", 
                                # "#bf5b17", "#666666")) +
            # labs(y = "%", x = "Environment", fill = "Area") +
            # theme_classic() +
            # theme(axis.title.x = element_text(face = "bold"),
                 #  axis.title.y = element_text(face = "bold"))}
#grey scale#
dados %>% 
  select(ua, environment, sound_type, area_group) %>% 
  gather(., key = "key", value = "value", area_group, sound_type) %>% 
  mutate(value = str_to_title(value)) %>% 
  filter(value == "All" |
           value == "Anthrophony" |
           value == "Biophony" |
           value == "Geophony") %>% 
  group_by(key, value) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(area_group) %>% 
  mutate(Perc = round(n*100/sum(n), 2)) %>% 
  {ggplot(., aes(x = environment, y = area_group)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      scale_fill_manual(labels = c("Total", "Acoustic Mapping"),
      values = c("#66c2a5", "#fc8d62")) +
      scale_fill_brewer(labels = c("Total", "Acoustic Mapping"),
                        palette = "Greys") +
      labs(y = "% of papers", x = "Sound type", fill = "Area") +
      theme_classic() +
      theme(axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position = "right") +
      ggsave("sound_grey.png", plot = last_plot(), dpi = 600, units = "cm", height = 7.5, width = 15.5)}




