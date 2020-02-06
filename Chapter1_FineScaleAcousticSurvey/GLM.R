#GLM to test which variables are the most important shaping the indices#
#Marina Scarpelli#
#07.01.2020#

library(tidyverse)
library(ggplot2)
library(stringi)
library(car)
library(data.table)

#Function that gives the path to the folder where the data is
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

#Reads the data
df_indices <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"))

df_vegdata <- read.csv(getDataPath("Fieldwork_Bowra", "27.08.2019_Data.csv"))

#Tidying up hobos data
hobos_data <- list.files(getDataPath("Fieldwork_Bowra", "Oct2019", "hobos_processed"), full.names = T)
for (file in hobos_data) {
  read.csv(file) %>% 
    separate(., col = 2, into = c("Date", "Time"), sep = " ") %>%
    write.csv(., file)
  }
  
files <- as.list(hobos_data)
df_hobos <- lapply(hobos_data, read.csv) 
df_hobos <- do.call(rbind, df_hobos)

#Joining indices and veg
df_IndicesAndVeg <- merge(x = df_indices, y = df_vegdata, by.x = "PointData", by.y = "Point")

#Joining hobo

df_hobos1 <- mutate(df_hobos, Date = case_when(df_hobos$Date == "10/14/19" ~ "20191014",
                                               df_hobos$Date == "10/15/19" ~ "20191015",
                                               df_hobos$Date == "10/16/19" ~ "20191016")) %>% 
  filter(., Date != "NA") %>% 
  mutate(., Time = gsub(x = Time, pattern = ":", replacement = ""))

changing_hour <- function(string, pattern, replacement) {
  mutate(df_IndicesAndVeg, "beginning_rec_modified" = str_replace_all(string, pattern, replacement))
}

changing_hour <- function(data, string, pattern, replacement) {
  return(mutate(data, "beginning_rec_modified" = str_replace_all(string, pattern, replacement)))
}
df_IndicesAndVeg6 <-  df_IndicesAndVeg6 %>% changing_hour(df_IndicesAndVeg6$beginning_rec_modified, "5929", "010000")

unique(df_IndicesAndVeg$beginning_rec)
unique(df_IndicesAndVeg6$beginning_rec_modified)
df_IndicesAndVeg6 <- df_IndicesAndVeg6 %>% changing_hour(df_IndicesAndVeg6$beginning_rec_modified, "5929", "010000")

write.csv(df_IndicesAndVeg6, getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation.csv"))

p <- unique(select(df_IndicesAndVeg6, beginning_rec_modified, beginning_rec))

rm(df_IndicesAndVeg1, df_IndicesAndVeg1, df_IndicesAndVeg2, df_IndicesAndVeg3, df_IndicesAndVeg4, df_IndicesAndVeg5)

df_IndicesAndVeg7 <- mutate(df_IndicesAndVeg6, new_ResultMinute = case_when(ResultMinute >= 0 & ResultMinute <= 9 ~ "00",
                                                                            ResultMinute >= 10 & ResultMinute <= 19 ~ "10",
                                                                            ResultMinute >= 20 & ResultMinute <= 29 ~ "20",
                                                                            ResultMinute >= 30 & ResultMinute <= 39 ~ "30",
                                                                            ResultMinute >= 40 & ResultMinute <= 49 ~ "40",
                                                                            ResultMinute >= 50 & ResultMinute <= 59 ~ "50"))
p <- unique(select(df_IndicesAndVeg6, new_ResultMinute, ResultMinute))
rm(p)

df_IndicesAndVeg8 <- mutate(df_IndicesAndVeg7, beginning_rec_modified = stri_sub_replace(df_IndicesAndVeg7$beginning_rec_modified, from = 3, to = 4, replacement = new_ResultMinute))

df_IndicesAndVeg9 <- mutate(df_IndicesAndVeg8, new_FID = paste(PointData, Date, beginning_rec_modified, sep = "_"))

df_hobos2 <- mutate(df_hobos1, new_FID = paste(point, Date, Time, sep = "_"))
df_hobos2 <- mutate(df_hobos2, Time = (as.numeric(Time)))

test <- case_when(str_match_all(df_hobos2$new_FID, df_IndicesAndVeg9$new_FID) ~ df_hobos2$temperature) 

df_complete <- merge(df_IndicesAndVeg9, df_hobos2, by = "new_FID", all.x = T)

  #write.csv(., getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation_completeDF.csv"))

df_complete <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation_completeDF.csv"))

explanatory_variables <- select(df_complete, X.1, beginning_rec_modified, NT_N_DIST, NT_N_HEIGHT, NT_S_DIST, NT_S_HEIGHT, NT_W_DIST, NT_W_HEIGHT, NT_E_DIST, NT_E_HEIGHT, NS_N_DIST, NS_N_HEIGHT, NS_S_DIST, NS_S_HEIGHT, NS_W_DIST, NS_W_HEIGHT, NS_E_DIST, NS_E_HEIGHT, GC_BS_E, GC_BS_N, GC_BS_S, GC_BS_W, GC_LT_E, GC_LT_W, GC_LT_N, GC_LT_S, GC_NF_E, GC_NF_S, GC_NF_W, GC_NF_N, GC_NG_E, GC_NG_S, GC_NG_N, GC_NG_W, GC_SH_E, GC_SH_N, GC_SH_S, GC_SH_W, CanopyCover, CanopyHeight, ShrubCover, SubcanopyHeight, temperature, humidity)
explanatory_variables[is.na(explanatory_variables)] <- 0

df_BGN <- cbind(explanatory_variables, BackrgoundNoise = (df_complete$BackgroundNoise))



#Background noise and buffer100####
model1 <- glm(df_BGN$BackgroundNoise ~ df_BGN[2:44], data = df_complete, family = gaussian()) #Here is the actual model. You'll put the response variable before the ~ and the explanatory variable(s) after it, adding variables with a + sign. We have a lot of GLM familys (gaussian, poisson and binomial). For continuous variables, you'll use the gaussian one; For counting data, Poisson; proportion data: binomial; Binary data: binomal. So you'll choose the most adequate one to you data.


#Okay, so you've ran the model and you don't actually sees the output. You have to run a null model, to compare with your model. Here is to check if your model is statistically different from the null. You'll put the response variable ~ and 1.

MN.BGN <- glm(df_complete$BackgroundNoise~1) 

#Now you run a test to check the difference between your model and the null.

anova(M.BGN100M, MN.BGN, test="F") #Use this one if you use the gaussian family
anova(M.BGN100M, MN.BGN, test="chisq") #Use this one if you use the poisson family

#Comparing the models: p>0.05 - your model is varying just like the null, i.e. your data and what happens by chance are the same, your variables aren't good to explain what you want. If your p<0.05: yeeey, congratulations, your variables are better than what happens randomly in nature :)
#If you come to this point, you'll leave the null model behind! Than, you do an anova inside your awesome model to check what variables really matter.

anova(M.BGN100M) #Check the significance of the variables. You'll try to make your model simpler from here. Pick up the meaningful variables and do it all over again. 

resultado3 <- glm(mydata$BackgroundNoise~mydata$BUF1k_C3_PERC, data = mydata) #Run the simpler model

anova(resultado3, resultado2,  test="F") #With the second (and simpler) model, test it against the previous one. If they are statistically the same, you can keep up with the simpler one. If not, you'll have to keep the most complex - and this is not good because models are a simplification of reality. So, the simpler, the better!


anova(resultado3, test="F") #Run the anova again with the new model, to check parameters and significance. If you can, make it simpler. Remove variables that are not meaningful and test it against the previous model until you get the simpler model you can.

#Once you've all that and you get your perfect model, you'll have to criticize it (i.e. check if the family you chose before is the most adequate one). You'll calculate the ratio between the residual deviance and the degrees of freedom - from residual deviance. The value of this ratio should be like one of the outcomes of the model: "Dispersion parameter for gaussian family taken to be...". This message you'll only appear once you've run the summary(result). If the result is similar, the distribution is adequate and you can move forward. If not, you'll have to play with the families to test which one is good for you - I haven't done the "playing" part, so I am hoping your distribution is adequate lol.
summary(resultado3)

420.71/55 #And you can do this in your own script, okay?

# So, now is the delicate part. I'll try to explain it to you the best I can, but let me know if yout need me to explain it in a different way and I'll try. Do the anova again (or just scroll the bar and get the parameters from the one you've done before lol). This is to calculate your pseudo R2, which is the explanation of your model - i.e. how much are your variables explaining the response. Calculate the ratio between test model deviance and the residual deviance of the null model (this parameters should appear in the ANOVA outcome.

53.296/474.01 #Pseudo R2: If the result is above 0.7 -> GREAT! Your variables explain 70% of your response, well done, good model! If the result is below 0.7 -> not so great. Your variables explain less than 70% of your response so maybe they are not the best ones, and you'll have to think about this carefully. Remember: Check the variable behaviour - if they are positively or negativaly related to your response. Also, there is a way to check the variables explanation of the model - deviance(model)/residual deviance(of the null) - for each variable.

#I think that's all you should know about it, let me know if you have more questions about it - or if I wasn't clear enough :) - Have fun!
