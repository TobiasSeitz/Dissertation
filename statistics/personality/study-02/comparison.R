# COMPARISON BETWEEN TWO PASSWORD TYPES
# FOLLOWS THE SAME STRUCTURE AS rating.R
library(mgcv)
library(ggplot2)
library(plyr)
library(texreg)
library(lme4)
library(reshape2)
library(visreg)


#source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

# read data from 100, cleaned dataset, preprocessed from excel.
d_comp100 <- read.csv("data-comparison-N100.csv", sep = ";", dec = ".")

### factorize things
d_comp100$D_Gender <- factor(d_comp100$D_Gender,levels=c("Male","Female"),labels=c("Male","Female"))
d_comp100$D_ComputerScienceBackground <- factor(d_comp100$D_ComputerScienceBackground, levels=c("No","Yes"), labels=c("No","Yes"))


responseVariablesComparison <- c("C_Length", "C_Strength",
                                 "C_Upper","C_Digits","C_Symbols",
                                 "C_ClassDelta"
                                 )


controlVariables <- list("D_Age","D_Gender","D_ComputerScienceBackground")
predictorsB5 <- list("B5_Extraversion","B5_Agreeableness", "B5_Conscientiousness", "B5_Neuroticism", "B5_Openness")
predictorsSeBIS <- list("SeBIS_Securement","SeBIS_Awareness", "SeBIS_Updating", "SeBIS_Passwords")
predictorsGDMS <- list("GDMS_Rational","GDMS_Intuitive", "GDMS_Avoidant",  "GDMS_Dependent", "GDMS_Spontaneous")


autoModelsComparisonB5 <- lapply(responseVariablesComparison, getSmoothedGAM, predictors=predictorsB5, d<-d_comp100, k=8)
#autoModelsComparisonGDMS <- lapply(responseVariablesComparison, getSmoothedGAM, predictors=predictorsGDMS, d<-d_comp100)
#autoModelsComparisonSeBIS <- lapply(responseVariablesComparison, getSmoothedGAM, predictors=predictorsSeBIS, d<-d_comp100)

updatedModelsComparisonB5 <- lapply(autoModelsComparisonB5, simplifyGAM)
#updatedModelsComparisonGDMS <- lapply(autoModelsComparisonGDMS, simplifyGAM)
#updatedModelsComparisonSeBIS <- lapply(autoModelsComparisonSeBIS, simplifyGAM)

# B5
lapply(updatedModelsComparisonB5, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsB5,
       prefix.predictors="comparison-b5-predictors-", prefix.control="comparison-b5-controls-",path="graphs/b5",xLab.predictors = "Trait Scores")
# GDMS
lapply(updatedModelsComparisonGDMS, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsGDMS,
       prefix.predictors="comparison-gdms-predictors-", prefix.control="comparison-gdms-controls-",path="graphs/gdms",xLab.predictors = "GDMS Scores")
# SeBIS
lapply(updatedModelsComparisonSeBIS, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsSeBIS,
       prefix.predictors="comparison-sebis-predictors-", prefix.control="comparison-sebis-controls-",path="graphs/sebis",xLab.predictors = "Sebis Scores")

## not using lapply now because of multi-core processor race conditions... ?
for(i in updatedModelsComparisonB5) {
  outputSummary(i, prefix="comparison-b5-", path="summaries/reml")
}
#for(i in updatedModelsComparisonGDMS) {
#  outputSummary(i, prefix="comparison-gdms-", path="summaries")
#}
#for(i in updatedModelsComparisonSeBIS){
#  outputSummary(i, prefix="comparison-sebis-", path="summaries")
#}

# to check autocorrelation (lines must not cross 0.2 by much)
#acf(residuals(updatedModelsComparisonB5[[1]]))
#pacf(residuals(updatedModelsComparisonB5[[1]]))

sink(file=NULL)
