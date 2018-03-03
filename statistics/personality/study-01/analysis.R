#install.packages("visreg")
#install.packages("texreg")
#install.packages('extrafont')

library(mgcv)
library(ggplot2)
library(plyr)
#package um latex tabellen auszugeben
library("texreg")
library(lme4)
library(reshape2)

# for confidence interval plots.
source("../../plotCI.R");
source('../../util.R')
source("plotGAM.R")

# BibTex citations
# print(citation(package = "mgcv"), bibtex = TRUE)

# import correct data sets
data.difficulty <- read.csv("data/data_creationDifficult_emoji12 (121 Datasets).CSV",
                  sep = ";", dec = ",")


# make colnames English
names(data.difficulty)[names(data.difficulty) == "Geschlecht"] <- "Gender"
names(data.difficulty)[names(data.difficulty) == "Alter"] <- "Age"
names(data.difficulty)[names(data.difficulty) == "Openess"] <- "Openness"
names(data.ratings)[names(data.ratings) == "Geschlecht"] <- "Gender"
names(data.ratings)[names(data.ratings) == "Alter"] <- "Age"
names(data.ratings)[names(data.ratings) == "Openess"] <- "Openness"

# give good labels to abbreviated column names
# TODO.
# positionLabels <- c("Task Number (emoji12)", "Task Number (2word12)", "Task Number (3class12)")
# labels(data.difficulty,which=c("emojiPos","twoWordPos","threeClassPos")) <- positionLabels;

# factorize data set of creation difficulty
data.difficulty$Gender[data.difficulty$Gender == 3] <- NA
data.difficulty$Gender <- factor(data.difficulty$Gender,levels=c(1,2),labels=c("Male","Female"))
data.difficulty$IT <- factor(data.difficulty$IT,levels=c(1,2),labels=c("Yes","No"))
data.difficulty$emojiPos <- factor(data.difficulty$emojiPos)
data.difficulty$twoWordPos <- factor(data.difficulty$twoWordPos)
data.difficulty$threeClassPos <- factor(data.difficulty$threeClassPos)
## calculate average difficulty
data.difficulty$meanDiff <- rowMeans(data.difficulty[which(colnames(data.difficulty) == "emojiDif" | colnames(data.difficulty) == "twoWordDif" | colnames(data.difficulty) == "threeClassDif")], na.rm=TRUE)
data.difficulty$meanTime <- (data.difficulty$emojiTime + data.difficulty$threeClassTime + data.difficulty$twoWordTime) / 3;

####  did conscientious people take more time for the first task?
data.difficulty$firstTime <- 0
data.difficulty$firstTime[data.difficulty$emojiPos==0] <- data.difficulty$emojiTime[data.difficulty$emojiPos==0]
data.difficulty$firstTime[data.difficulty$twoWordPos==0] <- data.difficulty$emojiTime[data.difficulty$twoWordPos==0]
data.difficulty$firstTime[data.difficulty$threeClassPos==0] <- data.difficulty$emojiTime[data.difficulty$threeClassPos==0]

### let's look at some descriptives first.

conditions <- c("emoji12", "2word12", "3class12");

### difficulty
difficultyTable <- data.frame(data.difficulty$emojiDif, data.difficulty$twoWordDif, data.difficulty$threeClassDif)
colnames(difficultyTable) <- conditions;
difficultyTable_long = melt(difficultyTable)
colnames(difficultyTable_long) <- c("Policy","DifficultyScore");
difficultyTable_long$Policy <- factor(difficultyTable_long$Policy)
difficultyTable_long$DifficultyScore <- factor(difficultyTable_long$DifficultyScore)

pDifficultyCI <- plotCI(
  difficultyTable_long,
  "DifficultyScore",
  "Policy",
  xAxisTitle="Difficulty Scores and 95% CIs")
pDifficultyCI <- pDifficultyCI + theme(text=element_text(size=20, family="Roboto Light")) # comment out if font not loaded
savePlot(pDifficultyCI,"difficulty-ci.pdf",8, height,path="graphs")

### timings
timingTable <- data.frame(data.difficulty$emojiTime,data.difficulty$twoWordTime,data.difficulty$threeClassTime);
colnames(timingTable) <- c("emoji12", "2word12", "3class12");
timingTable_long <- melt(timingTable);
colnames(timingTable_long) <- c("Policy","TimeToCreate")
timingTable_long$TimeToCreate <- timingTable_long$TimeToCreate/1000

pTimingCI <- plotCI(timingTable_long,
                    dependentVariable = "TimeToCreate",
                    groupVariable = "Policy",
                    xAxisTitle = "Time taken to create password (seconds) and 95% CIs", 
                    minValue = 40,
                    maxValue = 110,
                    step=10)
pTimingCI <- pTimingCI + theme(text=element_text(size=20, family="Roboto Light")) # comment out if font not loaded
savePlot(pTimingCI, "timing-ci", 8,height,path="graphs")


#meanDifficulty <- gam(meanDiff ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
#                        s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
#                     data = data.difficulty)
#summary(meanDifficulty)
#plot(meanDifficulty, pages = 1, jit=TRUE, scale = 0)
# Model for creationDifficulty -- emoiji12 ----------------------------------------------------------
# Assume non-linear effects for all continous variables (all but gender, IT, and emojiPos)

emojiDifficulty <- gam(emojiDif ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
              s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
            data = data.difficulty,method = "REML")
summary(emojiDifficulty)
texreg(emojiDifficulty)
outputSummary(emojiDifficulty,"difficulty-","summaries")
generatePDF(emojiDifficulty,controlVariables = list("Age","Gender","IT","emojiPos"), predictors = predictors, xLab.predictors = "Trait Scores")
#plot(emojiDifficulty, pages = 1, jit=TRUE, scale = 0)

## for plot output
width <- 10
height <- 3.0

### use GGPLOT to plot the GAMs
emojiDifficultyPlots <- plotGAM(emojiDifficulty,"emojiPos","Difficulty to create emoji12 password");
ggsave(plot=emojiDifficultyPlots[[1]], filename="dc-emoji-b5-plotGAM.pdf", path="graphs", width=width, height=height)
ggsave(plot=emojiDifficultyPlots[[2]], filename="dc-emoji-control.pdf", path="graphs", width=width, height=height)

#######
####### 
# if the summary of the "approximate significance of smooth terms" has an 
# estimated degrees of freedom (edf) greater than 1, it means that there is a non-linear effect / curve.
# in that case it could make sense to look at the curves in higher detail. 
#emoji
model1_emoji <- gam(emojiDif ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
                Conscientiousness + Neuroticism + s(Openness) + emojiPos,
              data = data.difficulty)
plot(model1_emoji, pages = 1, jit=TRUE)
summary(model1_emoji)
outputSummary(model1_emoji,"simple-","summaries")
gam.check(emojiDifficulty)

######
#twoWord Difficulty
######

twoWordDifficulty <- gam(twoWordDif ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                        s(Conscientiousness) + s(Neuroticism) + s(Openness) + twoWordPos,
                      data = data.difficulty)
summary(twoWordDifficulty)
plot(twoWordDifficulty, pages = 1, jit=TRUE, scale = 0)
twoWordDifficultyPlots <- plotGAM(twoWordDifficulty,"twoWordPos","Difficulty to create 2word12 password");
ggsave(plot=twoWordDifficultyPlots[[1]], filename="dc-2word-b5.pdf", path="graphs", width=width, height=height)
ggsave(plot=twoWordDifficultyPlots[[2]], filename="dc-2word-control.pdf", path="graphs", width=width, height=height)

######
# threeClass
######
threeClassDifficulty <- gam(threeClassDif ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
                        Conscientiousness + Neuroticism + s(Openness) + threeClassPos,
                      data = data.difficulty)
summary(threeClassDifficulty)
plot(threeClassDifficulty, pages = 1, jit=TRUE, scale =0)
threeClassDifficultyPlots <- plotGAM(threeClassDifficulty,"threeClassPos","Difficulty to create 3class12 password");
ggsave(plot=threeClassDifficultyPlots[[1]], filename="dc-3class-b5.pdf", path="graphs", width=width, height=height)
ggsave(plot=threeClassDifficultyPlots[[2]], filename="dc-3class-control.pdf", path="graphs", width=width, height=height)

# Code fuer Interaktionen:
# lineare Effekte: Neuroticism*Gender
# nichtlineare Effekte: s(Neuroticism, by = Gender)
model2 <- gam(emojiDif ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
               Conscientiousness + Neuroticism + s(Openness) + emojiPos,
             data = data.difficulty)
plot(model2, pages = 1, jit=TRUE)
summary(model2)


texreg(list(model1_emoji, twoWordDifficulty, threeClassDifficulty))
pModelComparisonplotreg <- plotreg(
  list(emojiDifficulty, twoWordDifficulty, threeClassDifficulty),
  custom.model.names = c("emoji12","2word12","3class12"),
  file="difficultyModelComparison.pdf"
  )
#############################################################################

data.difficulty$emojiTime = data.difficulty$emojiTime / 1000
data.difficulty$twoWordTime = data.difficulty$twoWordTime / 1000
data.difficulty$threeClassTime = data.difficulty$threeClassTime / 1000

emojiTimingModel <- gam(emojiTime ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                         s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
                       data = data.difficulty)
summary(emojiTimingModel)
plot(emojiTimingModel, pages = 1, jit=TRUE)

emojiTimingPlots <- plotGAM(emojiTimingModel,"emojiPos",yLab = "Time to create emoji12 password (s)")

ggsave(plot=emojiTimingPlots[[1]], filename="timing-emoji-b5.pdf", path="graphs", width=width, height=height)
ggsave(plot=emojiTimingPlots[[2]], filename="timing-emoji-control.pdf", path="graphs", width=width, height=height)

threeClassTimingModel <- gam(threeClassTime ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                               s(Conscientiousness) + s(Neuroticism) + s(Openness) + threeClassPos,
                             data = data.difficulty)
twoWordTimingModel <- gam(twoWordTime ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                            s(Conscientiousness) + s(Neuroticism) + s(Openness) + twoWordPos,
                          data = data.difficulty)
threeClassTimingPlots <- plotGAM(threeClassTimingModel,'threeClassPos',yLab = "Time to create 3class12 password (s)")
twoWordTimingPlots <- plotGAM(twoWordTimingModel, 'twoWordPos',yLab = "Time to create 2word12 password (s)")

ggsave(plot=threeClassTimingPlots[[1]], filename="timing-3class-b5.pdf", path="graphs", width=width, height=height)
ggsave(plot=threeClassTimingPlots[[2]], filename="timing-3class-control.pdf", path="graphs", width=width, height=height)
ggsave(plot=twoWordTimingPlots[[1]], filename="timing-2word-b5.pdf", path="graphs", width=width, height=height)
ggsave(plot=twoWordTimingPlots[[2]], filename="timing-2word-control.pdf", path="graphs", width=width, height=height)

summary(threeClassTimingModel)
plotreg(
  list(emojiTimingModel, twoWordTimingModel, threeClassTimingModel),
  custom.model.names = c("emoji12","2word12","3class12"),
  file="timingModelComparison.pdf"
)


#### using linear models for TIMING

emojiTimingModel_linear <- gam(emojiTime ~ Age + Gender + IT + s(Extraversion) + s(Agreeableness) +
                          Conscientiousness + s(Neuroticism) + s(Openness) + emojiPos,
                        data = data.difficulty)
threeClassTimingModel_linear <- gam(threeClassTime ~ s(Age) + Gender + IT + Extraversion + Agreeableness +
      Conscientiousness + Neuroticism + Openness + threeClassPos,
    data = data.difficulty)
twoWordTimingModel_linear <- gam(twoWordTime ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
                            Conscientiousness + s(Neuroticism) + Openness + twoWordPos,
                          data = data.difficulty)

summary(emojiTimingModel_linear)
summary(threeClassTimingModel_linear)
summary(twoWordTimingModel_linear)

texreg(
  list(emojiTimingModel_linear, twoWordTimingModel_linear, threeClassTimingModel_linear),
  custom.model.names = c("emoji12","2word12","3class12")
)
plotreg(
  list(emojiTimingModel_linear, twoWordTimingModel_linear, threeClassTimingModel_linear),
  custom.model.names = c("emoji12","2word12","3class12"),
  file="timingModelComparison.pdf"
)




#############################################################################
## 
wilcox.test(meanTime ~ Gender, data= data.difficulty, na.action = "omit")
ggplot(data.difficulty,aes(Gender,meanTime)) + geom_boxplot()
wilcox.test(twoWordTime ~ Gender, data= data.difficulty)

#############################################################################
##  automate stuff for difficulty and timing
#############################################################################


responsesGaussian <- list("emojiDif","twoWordDif","threeClassDif","meanTime","firstTime");
predictors <- list("Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism")
controls <- list("Age","Gender","IT") # important: start with a continuous variable, age is fine right now.
autoModelsUsability <- lapply(responsesGaussian, 
                              getGAM, 
                              predictors=predictors, 
                              controls=controls, 
                              controls.smoothed = list("Age"),
                              d <- data.difficulty,
                              k = 10
                              )
autoModelsUsability_simple <- lapply(autoModelsUsability, simplifyGAM,k=10, method="GCV.Cp")

source('../../plotGAM.R')
### plot 
lapply(autoModelsUsability_simple, generatePDF, 
       controlVariables = controls, predictors = predictors,
       prefix.predictors="predictors-", prefix.control="controls-",path="graphs/auto/GCV",xLab.predictors = "Trait Scores")

## summaries
for(i in autoModelsUsability_simple){
  outputSummary(i,prefix="auto-",path="summaries")
}



