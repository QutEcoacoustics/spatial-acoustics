library(tidyverse)
#User cases - GDM 

int_growth1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_intermediate_growth_1_1792-20220406-020745.csv") %>% 
  mutate(long = 145.525884,
         lat = -27.992356,
         treatment = "intermediate",
         point_name = "int_growth1")


int_growth2 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_intermediate_growth_2_1793-20220406-020815.csv")%>% 
  mutate(long = 145.528043,
         lat = -27.984654,
         treatment = "intermediate",
         point_name = "int_growth2")

int_growth3 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_intermediate_growth_3_1794-20220406-020831.csv") %>% 
  mutate(long = 145.537262,
         lat = -27.956989,
         treatment = "intermediate",
         point_name = "int_growth3")


old_mulga1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_old_mulga_1_1750-20220406-022927.csv") %>% 
  mutate(long = 145.57821,
         lat = -27.958167,
         treatment = "old",
         point_name = "old_mulga1")

old_mulga2 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_old_mulga_2_1790-20220406-020854.csv") %>% 
  mutate(long = 145.598853,
         lat = -27.985068,
         treatment = "old",
         point_name = "old_mulga2")

old_mulga3 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_old_mulga_3_1791-20220406-020912.csv") %>% 
  mutate(long = 145.557665,
         lat = -28.007207,
         treatment = "old",
         point_name = "old_mulga3")

regrowth_mulga1 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_regrowth_mulga_1_1796-20220406-020928.csv") %>% 
  mutate(long = 145.524935,
         lat = -27.97545,
         treatment = "regrowth",
         point_name = "regrowth_mulga1")

regrowth_mulga2 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_regrowth_mulga_2_1797-20220406-020941.csv") %>% 
  mutate(long = 145.528821,
         lat = -27.970789,
         treatment = "regrowth",
         point_name = "regrowth_mulga2")

regrowth_mulga3 <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/cunnamulla_1088_regrowth_mulga_3_1798-20220406-020956.csv") %>% 
  mutate(long = 145.513344,
         lat = -27.968159,
         treatment = "regrowth",
         point_name = "regrowth_mulga3")

complete_df <- rbind(int_growth1, int_growth2, int_growth3, old_mulga1, old_mulga2, old_mulga3, regrowth_mulga1, regrowth_mulga2, regrowth_mulga3)

complete_df <- select(complete_df, event_duration_seconds, low_frequency_hertz, high_frequency_hertz, common_name_tags, other_tags, event_start_date_australia_brisbane_10_00, event_start_time_australia_brisbane_10_00, listen_url, long, lat, treatment, point_name) %>% 
  separate(common_name_tags, into = c("tag_number", "common_name"), sep = ":", remove = F) %>% 
  filter(common_name != "NA" & common_name != "goat") %>% 
  droplevels(.)

complete_df$common_name <- as.factor(complete_df$common_name)

feeding <- read.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/birds_feedinghabits.csv") 

complete_df$common_name <- tolower(complete_df$common_name)
complete_df$common_name <- str_replace(complete_df$common_name, "-", " ")
complete_df$common_name <- str_remove(complete_df$common_name, "'")
complete_df <- mutate(complete_df, common_name = case_when(common_name == "grey shike thrush" ~ "grey shrike thrush",
                         TRUE ~ common_name))
feeding$Row.Labels <- tolower(feeding$Row.Labels)
feeding$Row.Labels <- str_replace(feeding$Row.Labels, "-", " ")
feeding$Row.Labels <- str_remove(feeding$Row.Labels, "'")

join <- right_join(feeding, complete_df, by = c("Row.Labels" = "common_name")) %>% 
  select(everything(), -c(X, X.1, X.2)) %>% 
  write.csv("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/_Soundscapes/Brendan_BowraData/final_df_feeding.csv", row.names = F)

library(gdm)


?formatsitepair
