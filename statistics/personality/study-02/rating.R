### STANDALONE STRENGTH RATING
library(mgcv)
library(ggplot2)
library(plyr)
library(texreg)
library(lme4)
library(reshape2)
library(visreg)


source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

# read data from 100, cleaned dataset, preprocessed from excel.
d100 <- read.csv("dataset-N100.csv", sep = ";", dec = ".")


### factorize things
d100$D_Gender <- factor(d100$D_Gender,levels=c("Male","Female"),labels=c("Male","Female"))
d100$D_ComputerScienceBackground <- factor(d100$D_ComputerScienceBackground, levels=c("No","Yes"), labels=c("No","Yes"))


testVariables <- c("G_Digits","G_Special","G_Uppercase")
responseVariablesRating <- c("G_Common","G_Passphrase","G_Pattern",
                                "G_LengthLong","G_LengthShort",
                                "G_Weak","G_Medium","G_Strong",
                                "G_Digits","G_Special","G_Uppercase")

controlVariables <- list("D_Age","D_Gender","D_ComputerScienceBackground")
predictorsB5 <- list("B5_Extraversion","B5_Agreeableness", "B5_Conscientiousness", "B5_Neuroticism", "B5_Openness")
predictorsSeBIS <- list("SeBIS_Securement","SeBIS_Awareness", "SeBIS_Updating", "SeBIS_Passwords")
predictorsGDMS <- list("GDMS_Rational","GDMS_Intuitive", "GDMS_Avoidant",  "GDMS_Dependent", "GDMS_Spontaneous")


###############################################################################################################################
### 
### Looped uncustomized stuff
### good to get started. 
###
###############################################################################################################################
# https://stackoverflow.com/a/30265548/1447479 
autoModelsRating <- lapply(responseVariablesRating,function(column){
  autoFormula <- as.formula(paste(column,"~ s(D_Age) + D_Gender + D_ComputerScienceBackground + s(B5_Extraversion) + s(B5_Agreeableness) +
                              s(B5_Conscientiousness) + s(B5_Neuroticism) + s(B5_Openness)"))
  autoGam <- gam(autoFormula, data=d100);
  autoGamPlots <- plotGAM(autoGam, controlVariables = controlVariables, predictors = predictorsB5, yLab=column, xLab.predictors = "Trait Score")
  autoFileNameB5 <- paste("rating-",column,"-b5.pdf",sep="")
  autoFileNameControl <- paste("rating-",column,"-control.pdf",sep="")
  savePlot(autoGamPlots[[1]],autoFileNameB5,path="graphs")
  savePlot(autoGamPlots[[2]],autoFileNameControl,path="graphs")
  autoGam
})
autoModelsRating[1]

###############################################################################################################################
### 
### Overall Strength Ratings
###
###############################################################################################################################
rOverallModel <- gam(G_Overall ~ s(D_Age) + D_Gender + D_ComputerScienceBackground + s(B5_Extraversion) + s(B5_Agreeableness) +
      s(B5_Conscientiousness) + s(B5_Neuroticism) + s(B5_Openness),
    data = d100)
# summary(rOverallModel)
## we can use the non-smoothed version where the estimated degrees of freedom of smooth terms == 1
## this will then give us correlation coefficients
rOverallModel_simple <- gam(G_Overall ~ D_Age + D_Gender + D_ComputerScienceBackground + 
                              B5_Extraversion + B5_Agreeableness +
                              B5_Conscientiousness + s(B5_Neuroticism) + 
                              B5_Openness,
                            data = d100) 
summary(rOverallModel_simple)

rOverallPlots <- plotGAM(rOverallModel_simple,controlVariables = controlVariables, predictors = predictorsB5,yLab = "Overall Strength Rating",xLab.predictors = "Trait Score")
savePlot(rOverallPlots[[1]],"rating-overall-b5.pdf",path="graphs")
savePlot(rOverallPlots[[2]],"rating-overall-control.pdf",path="graphs")



###############################################################################################################################
### 
### Overall Strength Ratings
###
###############################################################################################################################


