### STANDALONE STRENGTH RATING
library(mgcv)
library(ggplot2)
library(plyr)
library(texreg)
library(lme4)
library(reshape2)
library(itsadug)
library(Hmisc)
library(psych)
library(lm.beta)

#source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

# read data from 100, cleaned dataset, preprocessed from excel.
d100 <- read.csv("dataset-N100-small.csv", sep = ";", dec = ".")
b5Items <- read.csv("b5-items.csv", sep = ";", dec = ".")

### factorize things
d100$D_Gender <- factor(d100$D_Gender,levels=c("Male","Female"),labels=c("Male","Female"))
d100$D_ComputerScienceBackground <- factor(d100$D_ComputerScienceBackground, levels=c("No","Yes"), labels=c("No","Yes"))

responseVariablesRating <- c("G_Overall", "G_Common","G_Passphrase","G_Pattern",
                             "G_LengthLong","G_LengthShort",
                             "G_Weak","G_Medium","G_Strong",
                             "G_Digits","G_Special","G_Uppercase")

controlVariables <- list("D_Age","D_Gender","D_ComputerScienceBackground")

predictorsB5 <- list("B5_Extraversion","B5_Agreeableness", "B5_Conscientiousness", "B5_Neuroticism", "B5_Openness")
predictorsSeBIS <- list("SeBIS_Securement","SeBIS_Awareness", "SeBIS_Updating", "SeBIS_Passwords")
predictorsGDMS <- list("GDMS_Rational","GDMS_Intuitive", "GDMS_Avoidant",  "GDMS_Dependent", "GDMS_Spontaneous")

responseFrame <- d100[,which(names(d100) %in% responseVariablesRating)]
b5Frame <- d100[,which(names(d100) %in% predictorsB5)]
gdmsFrame <- d100[,which(names(d100) %in% predictorsGDMS)]
sebisFrame <- d100[,which(names(d100) %in% predictorsSeBIS)]


### ok, let's do this.
# 1. descriptives.
# 1a.normalize response thingies.
# 2. detect and remove outliers in each response variable. 
# 3. perform factor analysis of the scales and/or their aggregated score.
# 4. add the factors to a data-frame that has only the control vars and responses. 
# 5. run regression using the aggregate scores as predictors
# 6. run regression using the underlying factors as predictors
# 7. plot and save
# 8. summarize and save


###############################################################################################################################
### 
### ASSUMPTION and Exploration
###
###############################################################################################################################

# 1. descriptives
# get a summary of descriptive statistics.
sink(file="descriptives.txt")
print(describe(d100))
sink(file=NULL)

# look at normality tests.
shapiro.test(d100$G_Overall)
ks.test(d100$G_Overall,rnorm(100, mean = mean(d100$G_Overall)))

# 1a. normalize
pointsPerItem <- 7;
gOverall <- 13;
gCommon <- 3; 
gPassphrase <- 2;
gPattern <- 3;
gLong <- 6;
gShort <- 7;
gWeak <- 3;
gMedium <- 4;
gStrong <- 5;
gDigits <- 8;
gSpecial <- 5;
gUppercase <- 5;

### only do this once!
dNormed <- d100;
dNormed$G_Overall = dNormed$G_Overall / gOverall;
dNormed$G_Common = dNormed$G_Common / gCommon;
dNormed$G_Passphrase = dNormed$G_Passphrase / gPassphrase;
dNormed$G_Pattern = dNormed$G_Pattern / gPattern;
dNormed$G_LengthLong = dNormed$G_LengthLong / gLong;
dNormed$G_LengthShort = dNormed$G_LengthShort / gShort;
dNormed$G_Weak = dNormed$G_Weak / gWeak;
dNormed$G_Medium = dNormed$G_Medium / gMedium;
dNormed$G_Strong = dNormed$G_Strong / gStrong;
dNormed$G_Digits = dNormed$G_Digits / gDigits;
dNormed$G_Special = dNormed$G_Special / gSpecial;
dNormed$G_Uppercase = dNormed$G_Uppercase / gUppercase;


# 2. outliers
#boxplot(d100$G_Overall, , main="Overall", boxwex=0.1)
dResponseLongForm <- melt(dNormed, id.vars = "X...id", measure.vars = responseVariablesRating);

(outlierPlot <- ggplot(dResponseLongForm) +
  geom_boxplot(aes(x=dResponseLongForm$X...id,y=value, color=variable)) +
   scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
   scale_x_discrete(labels = c("Overall","Common","Passphrase","Pattern","Long","Short","Weak","Medium","Strong","Digits","Symbols","Uppercase")) +
   coord_flip() +
   guides(colour = guide_legend(reverse=T)) +
   labs(y="Average Rating",x="Password Group")
)
outlierPlot
savePlot(outlierPlot,"avearge-rating-boxplot.pdf",path="graphs",height=4)


# derive components
pcB5 <- princomp(b5Items[,2:ncol(b5Items)]) # omit ID variable
summary(pcB5)
plot(pcB5)

# pca plot showed 10 components (i.e. a lot more than the 5 we'd expect)
# factor analysis
b5FactorScores <- factanal(b5Items[,2:ncol(b5Items)],factors=10,rotation="varimax",scores="regression")$scores
b5FactorScores <- as.data.frame(b5FactorScores)

faB5 <- data.frame(d100,b5FactorScores)
predictorsFA <- as.list(names(b5FactorScores))

pdf("panels.pdf")
pairs.panels(responseFrame,pch=".")
pairs.panels(b5Frame)
pairs.panels(gdmsFrame)
pairs.panels(sebisFrame)
dev.off()


pdf("correlations.pdf")
lapply(list(responseFrame,b5Frame,gdmsFrame,sebisFrame),cor.plot,number=TRUE)
dev.off()


fa(b5Frame,2,n.obs = 100,fm="wls")

allPredictors <- c(predictorsB5,predictorsGDMS)

#test
(testPlot <- ggplot(data=d100, aes(x=B5_Openness,y=G_Overall)) + 
  geom_point() +
    geom_smooth(method="loess")
  )

db5 <- d100[,which(names(d100) %in% predictorsB5)]
res <- rcorr(as.matrix(db5))
corrplot(as.matrix(res), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
res


###############################################################################################################################
### 
### Looped uncustomized stuff
### good to get started. 
###
###############################################################################################################################

autoModelsRatingB5 <- lapply(responseVariablesRating, getSmoothedGAM,predictors=predictorsB5,d<-d100, select=FALSE,method="REML")
autoModelsRatingGDMS <- lapply(responseVariablesRating, getSmoothedGAM,predictors=predictorsGDMS,d<-d100)
autoModelsRatingSeBIS <- lapply(responseVariablesRating, getSmoothedGAM,predictors=predictorsSeBIS,d<-d100)
autoModelsRatingFA <- lapply(responseVariablesRating, getSmoothedGAM,predictors=predictorsFA,d<-faB5,method="REML")

#autoModelsAll <- lapply(responseVariablesRating, getSmoothedGAM, predictors=allPredictors,d<-d100)

## we now use select=TRUE in gam() to add more penalty to terms. So we don't need to simplify anymore.
updatedModelsB5 <- lapply(autoModelsRatingB5, simplifyGAM,method="REML");
updatedModelsFA <- lapply(autoModelsRatingFA, simplifyGAM,method="REML")
#updatedModelsSeBIS <- lapply(autoModelsRatingSeBIS, simplifyGAM);
#updatedModelsGDMS <- lapply(autoModelsRatingGDMS, simplifyGAM);
#updatedModelsAll <- lapply(autoModelsAll,simplifyGAM)


#####
# plot everything
#####
# B5
lapply(updatedModelsB5, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsB5,
       prefix.predictors="rating-b5-predictors-", prefix.control="rating-b5-controls-",path="graphs/b5-reml",xLab.predictors = "Trait Scores")
# FA
lapply(updatedModelsFA, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsFA,
       prefix.predictors="rating-b5-predictors-", prefix.control="rating-b5-controls-",path="graphs/fa-reml",xLab.predictors = "Trait Scores")

# GDMS
lapply(autoModelsRatingGDMS, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsGDMS,
       prefix.predictors="rating-gdms-predictors-", prefix.control="rating-gdms-controls-",path="graphs/gdms-penalty",xLab.predictors = "GDMS Scores")
# SeBIS
lapply(autoModelsRatingSeBIS, generatePDF, 
       controlVariables = controlVariables, predictors = predictorsSeBIS,
       prefix.predictors="rating-sebis-predictors-", prefix.control="rating-sebis-controls-",path="graphs/sebis-penalty",xLab.predictors = "Sebis Scores")
# ALL
#lapply(updatedModelsAll, generatePDF, 
#       controlVariables = controlVariables, predictors = allPredictors,
#       prefix.predictors="rating-all-predictors-", prefix.control="rating-all-controls-",path="graphs/all")


#####
# create text summaries.
#####

allUpdatedModels <- c(updatedModelsB5,updatedModelsGDMS, updatedModelsSeBIS);


## not using lapply now because of multi-core processor race conditions... ?
for(i in updatedModelsB5) {
  outputSummary(i, prefix="b5-", path="summaries/reml")
}

for(i in updatedModelsFA) {
  outputSummary(i, prefix="fa-", path="summaries/fa-reml")
}
for(i in autoModelsRatingGDMS) {
  outputSummary(i, prefix="gdms-", path="summaries/penalized")
}
for(i in autoModelsRatingSeBIS){
  outputSummary(i, prefix="sebis-", path="summaries/penalized")
}
#for(i in updatedModelsAll){
#  outputSummary(i, prefix="all-", path="summaries")
#}

for(i in autoModelsAll){
  outputSummary(i, prefix="autoall-", path="summaries")
}

sink(file=NULL)


###############################################################################################################################
###
### FROM PCA/Factor analysis
###
###############################################################################################################################
rFactorModel <- getSmoothedGAM("G_Overall",predictorsFA,k=5,d<-faB5,select=FALSE,method="REML")
rFactorModel_simple <- simplifyGAM(rFactorModel)
rFactorModel_simple_GCV <- simplifyGAM(rFactorModel,method="GCV.Cp",select=TRUE)
rFactorModel_manual <- gam(G_Overall ~ + s(Factor3, k = 5) + s(Factor4, k = 5) + D_Gender +  D_ComputerScienceBackground, method="REML",data=faB5)

summary(rFactorModel)
summary(rFactorModel_simple)
summary(rFactorModel_manual)
gam.check(rFactorModel)
gam.check(rFactorModel_simple)
gam.check(rFactorModel_manual)
plotGAM(rFactorModel,controlVariables,predictorsFA)[[1]]
plotGAM(rFactorModel_simple,controlVariables,predictorsFA)[[1]]
plotGAM(rFactorModel_manual,controlVariables,predictorsFA)[[1]]

#
AIC(rFactorModel, rFactorModel_simple, rFactorModel_simple_GCV, rFactorModel_manual, autoModelsRatingB5[[1]], simplifyGAM(autoModelsRatingB5[[1]],method="GCV.Cp"), simplifyGAM(rFactorModel,method="GCV.Cp"))
anova(autoModelsRatingB5[[1]],updatedModelsB5[[1]],test='F')

cooks.distance(autoModelsRatingB5[[1]],updatedModelsB5[[1]])





###############################################################################################################################
###
### Example for handy-work 
### Overall Strength Ratings
###
###############################################################################################################################
rOverallModel <- gamm(G_Overall ~ s(B5_Extraversion) + s(B5_Agreeableness) +
                       s(B5_Conscientiousness) + s(B5_Neuroticism) + s(B5_Openness) +
                        s(D_Age) + D_Gender + D_ComputerScienceBackground ,
                     #select = TRUE,
                     data = dNormed)
summary(rOverallModel)
gam.check(rOverallModel)
plotGAM(rOverallModel,controlVariables,predictorsB5)[[1]]
 ## we can use the non-smoothed version where the estimated degrees of freedom of smooth terms == 1
## this will then give us correlation coefficients
rOverallModel_simple <- gam(G_Overall ~ D_Age + D_Gender + D_ComputerScienceBackground + 
                              B5_Extraversion + B5_Agreeableness +
                              B5_Conscientiousness + s(B5_Neuroticism) + 
                              B5_Openness,
                            data = d100) 
rOverallModel_simple <- simplifyGAM(rOverallModel);
summary(rOverallModel_simple)
#c(updatedModelsB5[[1]]$gcv.ubre,rOverallModel$gcv.ubre,rOverallModel_simple$gcv.ubre)
for(i in 1:length(autoModelsRatingB5)){
  print(c(updatedModelsB5[[i]]$gcv.ubre,autoModelsRatingB5[[i]]$gcv.ubre))
}

rOverallPlots <- plotGAM(rOverallModel,controlVariables = controlVariables, predictors = predictorsB5,yLab = "Overall Strength Rating",xLab.predictors = "Trait Score")
savePlot(rOverallPlots[[1]],"rating-overall-b5.pdf",path="graphs/test")
savePlot(rOverallPlots[[2]],"rating-overall-control.pdf",path="graphs/test")


###############################################################################################################################
###
### Much smoothed.
### Overall Strength Ratings
###
###############################################################################################################################
rLinModel <- lm(G_Overall ~ D_Age + D_Gender + D_ComputerScienceBackground + B5_Extraversion + B5_Agreeableness +
                  B5_Conscientiousness + B5_Neuroticism + B5_Openness,
                data = d100)
summary(rLinModel)

rSmoothedModelB5 <- gam(G_Overall ~ D_Age + D_Gender + D_ComputerScienceBackground + B5_Extraversion + B5_Agreeableness +
                          B5_Conscientiousness + B5_Neuroticism + B5_Openness,
                        method="REML",
                        select=TRUE,
                        data = d100)

summary(rSmoothedModelB5)

rOverallPlots <- plotGAM(rOverallModel_simple,controlVariables = controlVariables, predictors = predictorsB5,yLab = "Overall Strength Rating",xLab.predictors = "Trait Score")
savePlot(rOverallPlots[[1]],"rating-overall-b5.pdf",path="graphs")
savePlot(rOverallPlots[[2]],"rating-overall-control.pdf",path="graphs")



###############################################################################################################################
###
### Example for handy-work 
### Overall Strength Ratings
###
###############################################################################################################################
rOverallModel <- gamm(G_Overall ~ s(B5_Extraversion) + s(B5_Agreeableness) +
                        s(B5_Conscientiousness) + s(B5_Neuroticism) + s(B5_Openness) +
                        s(D_Age) + D_Gender + D_ComputerScienceBackground ,
                      #select = TRUE,
                      data = dNormed)



