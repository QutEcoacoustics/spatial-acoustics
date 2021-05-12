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
