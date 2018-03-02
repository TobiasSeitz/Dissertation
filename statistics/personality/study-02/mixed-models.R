#data transformation

library(mgcv)
library(plyr)
library(reshape2)
library(texreg)
library(itsadug)

#source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

# read data from 100, cleaned dataset, preprocessed from excel.
d <- read.csv("data-mixed-model.csv", sep = ";", dec = ".",check.names=TRUE)
pwAnalysis <- read.csv("pw-analysis.csv", sep = ",", dec = ".")
pwAnalysis <- pwAnalysis[,1:12] # we only need the first 12 columns

idVars <- c(
  "X...subject",
  "D_Gender",
  "D_Age",
  "D_ComputerScienceBackground",
  "B5_Openness",
  "B5_Conscientiousness",
  "B5_Extraversion",
  "B5_Agreeableness",
  "B5_Neuroticism"
)

measureVars <- names(d[!names(d) %in% idVars])
dLong <- melt(d, id.vars = , measure.vars = measureVars)
dLong <- rename(dLong, c("variable"="password","value"="rating")) # requires plyr
dLong$password <- as.character(dLong$password)
dLong$password[dLong$password=="X.thedzfhg123"] = "thedzfhg123"
dLong$password[dLong$password=="X11Nd1sPPut8ble99"] = "11Nd1sPPut8ble99"
dLong$password[dLong$password=="F.m1Ly07."] = "F@m1Ly07%"
dLong$password[dLong$password=="bicycles.peaches.cold"] = "bicycles-peaches-cold"
dLong$password[dLong$password=="ocean4.Size..beer.Car"] = "ocean4 Size !beer Car"
dLong$password[dLong$password=="AatIcs.ijayl.t"] = "AatIcs,ijayl-t"
dLong$password[dLong$password=="X1qaz2wsx3edc"] = "1qaz2wsx3edc"
dLong$password[dLong$password=="p.ssw0rd"] = "p@ssw0rd"
dLong$password <- factor(dLong$password)

# plot strength ratings. 
(boxPlotRatings <- ggplot(dLong) +
  geom_boxplot(aes(x=password,y=rating,color=password)) +
    scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
    scale_x_discrete(labels=c()) + 
    #coord_flip() +
    #guides(colour = guide_legend(reverse=T)) +
    labs(y="Perceived Strength",x="Password",col="Passwords")
  )
savePlot(boxPlotRatings,filename="boxplot-ratings-pws-v3.pdf",width=10,height=4,path="graphs")


merged <- merge(x=dLong,y=pwAnalysis,by.x="password",by.y="password",all.x=TRUE)

predictorsB5 <- list(  "B5_Openness",
                       "B5_Conscientiousness",
                       "B5_Extraversion",
                       "B5_Agreeableness",
                       "B5_Neuroticism")

predictorsFeatures <- list("digits","symbols","uppercase","lowercase");

predictorsMixedModel <- c(predictorsFeatures,lapply(predictorsB5,smoothPredictors,k=10))
predictorsMixedGraph <- c(predictorsFeatures,predictorsB5)

predictorString <- paste(predictorsMixedModel,collapse = "+");
autoFormula <- as.formula(paste("rating",predictorString,sep = "~"))
featuresModel <- gam(autoFormula, data=merged, method="REML")
featuresModel_simple <- simplifyGAM(featuresModel)
summary(featuresModel)
summary(featuresModel_simple)
#summary(lm(autoFormula,data=merged))
outputSummary(featuresModel_simple,prefix="rating-simple-b5-", path="summaries/mixed")
plot(featuresModel_simple,jit=TRUE,pages=1)


mixedModelPlot <- plotGAM(featuresModel_simple,predictors=predictorsMixedGraph)
mixedModelPlot <- plotGAM(featuresModel, predictors=predictorsFeatures,controlVariables = predictorsB5)
savePlot(mixedModelPlot[[1]],filename="rating-mixed-v2-predictors.pdf",path="graphs/mixed")
savePlot(mixedModelPlot[[2]],filename="rating-mixed-v2-b5.pdf",path="graphs/mixed")
#savePlot(boxPlotRatings[[2]],filename="rating-mixed-control.pdf",path="graphs/mixed")
mixedModelPlot[[1]]



#################################################################### manual gam
# manual simple gam
####################################################################
simpleFeaturesGam <- gam(rating ~ lowercase + uppercase + substitutions, method="REML",data=merged)
summary(simpleFeaturesGam)
outputSummary(simpleFeaturesGam,prefix="simple-",path="summaries/mixed")
manualSimplePlot <- plotGAM(simpleFeaturesGam,predictors=list("digits","length","lowercase","symbols","uppercase"), 
                            xLab.predictors = "Feature Count",
                            yLab = "Password Rating",
                            plotResiduals = FALSE
)
savePlot(manualSimplePlot[[1]],filename="rating-manual-simple.pdf",path="graphs/mixed")

gamtabs(simpleFeaturesGam)


# manual mixed gam
####################################################################
manualMixedGam <- gam(rating ~ (digits * length) + (lowercase * length) + (symbols * length) + 
                        (uppercase * length) + (digits*substitutions*symbols) +
                        s(B5_Openness,k=7) + 
                        B5_Agreeableness + 
                        s(B5_Neuroticism,k=9), 
                        method="REML", data = merged)
summary(manualMixedGam)
outputSummary(manualMixedGam,prefix="09-",path="summaries/mixed")
manualMixedPlot <- plotGAM(manualMixedGam,
                           predictors=list("digits","length","lowercase","symbols","uppercase","chunks","substitutions","score"), 
                           controlVariables = predictorsB5,
                           xLab.predictors = "Feature Count",
                           xLab.control = "Big Five Trait Scores",
                           yLab = "Password Rating",
                           plotResiduals = T
                           )
savePlot(manualMixedPlot[[1]],filename="rating-manual-mixed-v9-predictors.pdf",path="graphs/mixed")
savePlot(manualMixedPlot[[2]],filename="rating-manual-mixed-v9-b5.pdf",path="graphs/mixed")
manualMixedPlot[[1]]
manualMixedPlot[[2]]

mergedNoLeet <- merged[merged$password!='F@m1Ly07%' & merged$password!='p@ssw0rd',]
manualMixedGam_noLeet <- gam(rating ~ (digits * length) + (lowercase * length) + (symbols * length) + 
                        (uppercase * length) + 
                        s(B5_Openness,k=7) + 
                        B5_Agreeableness + 
                        s(B5_Neuroticism,k=9)
                      , method="REML", data = mergedNoLeet)
summary(manualMixedGam_noLeet)
outputSummary(manualMixedGam,prefix="06-noLeet-",path="summaries/mixed")

## compare fits
AIC(featuresModel,featuresModel_simple,simpleFeaturesGam, manualMixedGam)
anova(featuresModel_simple,manualMixedGam, test="F")
plotreg(
  list(featuresModel_simple, manualMixedGam),
        custom.model.names = c("Features + B5","Features + B5 (with interactions on length)"),
        file="graphs/mixed/comparison.pdf"
)

sink(file="summaries/mixed/final-model.tex")
texreg()

###############################################################################################################################
### using guesses-log10 as strength metric.
###############################################################################################################################
continuousStrengthModel <- getSmoothedGAM("guesses_log10")
