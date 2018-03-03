source("../../plotGAM.R");
source("../../util.R");

# meta
d100 <- read.csv("dataset-N100-small.csv", sep = ";", dec = ".")
d100$D_Gender <- factor(d100$D_Gender,levels=c("Male","Female"),labels=c("Male","Female"))
d100$D_ComputerScienceBackground <- factor(d100$D_ComputerScienceBackground, levels=c("No","Yes"), labels=c("No","Yes"))


predictorsMeta <- list("Meta_Length","Meta_Lowercase","Meta_Uppercase","Meta_Digits","Meta_Special");
controlVariables <- list("D_Age","D_Gender","D_ComputerScienceBackground")
responseVariablesRating <- c("G_Overall", "G_Common","G_Passphrase","G_Pattern",
                             "G_LengthLong","G_LengthShort",
                             "G_Weak","G_Medium","G_Strong",
                             "G_Digits","G_Special","G_Uppercase")
autoModelsRatingMeta <- lapply(responseVariablesRating, getSmoothedGAM,predictors=predictorsMeta,d<-d100, select=FALSE,method="REML",k=3)
updatedModelsMeta <- lapply(autoModelsRatingMeta, simplifyGAM,method="REML")

# plot 
lapply(updatedModelsMeta, generatePDF, 
       controlVariables = controlVariables, 
       predictors = predictorsMeta,
       prefix.predictors="rating-meta-predictors-", 
       prefix.control="rating-meta-controls-",
       path="graphs/meta-reml",
       xLab.predictors = "Meta Password Statistics")

for(i in updatedModelsMeta){
  outputSummary(i, path="summaries/meta")
}


