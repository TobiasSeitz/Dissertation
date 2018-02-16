# management / coping strategy

library(mgcv)
library(ggplot2)
library(reshape2)
library(corrplot)

source("../../plotGAM.R");
source("../../util.R");

# this data looks stale: d <- read.csv("personality-pw-selection.csv", sep = ";", dec = ".")
# I've re-coded it:
d <- read.csv("data/data_recoded.csv", sep = ";", dec = ".")

# avoid that the the one third gender is factored into the models
d$gender[d$gender == 3] <- NA
d$gender <- factor(d$gender, levels = c(1,2), labels=c("Male","Female"))
d$writeDown <- factor(d$writeDown, levels=c(1,2,3),labels=c("Password Manager","Paper","Memorized"))


############# Predictors
predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");
controlVars <- list("age","gender","it_background","occupation")

############# Management
# show a histogram of strategies.
ggplot(d) + geom_histogram(aes(x=writeDown,colour=writeDown),stat = "count")
count(d$writeDown)

# we need to transform the categorical data into binary. 
pwmMask <- c(1,0,0)
paperMask <- c(0,1,0)
memoMask <- c(0,0,1)
d$cope_pwm <- pwmMask[d$writeDown]
d$cope_paper <- paperMask[d$writeDown]
d$cope_memorize <- memoMask[d$writeDown]

### reponse variables now all in binary format
binaryResponseVariables <- list("reuse", "cope_pwm","cope_paper","cope_memorize")

autoModelsCoping <- lapply(binaryResponseVariables, getGam, d<-d, 
                          controlVars <- controlVars,
                          predictors = predictorsB5,
                          family="binomial")
autoModelsCoping_simple <- lapply(autoModelsCoping,simplifyGAM,family="binomial")

# test
plot(autoModelsCoping[[2]],jit=T,pages=1);
### plot 
lapply(autoModelsCoping, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="coping-b5-predictors-", prefix.control="coping-b5-controls-",path="graphs/b5-reml",xLab.predictors = "Trait Scores")

### plot 
lapply(autoModelsCoping_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="coping-simple-b5-predictors-", prefix.control="coping-simple-b5-controls-",path="graphs/b5-reml",xLab.predictors = "Trait Scores")

## summaries
for(i in autoModelsCoping){
  outputSummary(i,prefix="coping-",path="summaries/coping")
}
for(i in autoModelsCoping_simple){
  outputSummary(i,prefix="coping-simple-",path="summaries/coping")
}

############# Intercorrelation
# so, it looks as though more open people are more likely to adopt a pwm and older people are also more likely to adopt a pwm.
# however, older people are not really more open - I think there is a tendency to become more conservative and thus less open. 
# so, younger people don't need a pwm but they're more open. Is there a sweet spot?
# what if we use these as dependent factors?
manualCVs <- list("age")
manualIVs <- list("Openness");


copingModelManual <- gam(cope_pwm ~ age*Openness, data=d, method="REML", family="binomial")
plotsPwm <- plotGAM(copingModelManual, manualCVs, manualIVs, yLab="Password Manager", 
                    xLab.predictors = "Openness", xLab.control = "Age")

## there is no visible difference with this model. 
copingModelManual <- gam(cope_paper ~ age*Openness, data=d, method="REML", family="binomial")
plotsPaper <- plotGAM(copingModelManual, manualCVs, manualIVs, yLab="Paper", 
                    xLab.predictors = "Openness", xLab.control = "Age")

copingModelManual <- gam(cope_memorize ~ age*Openness, data=d, method="REML", family="binomial")
plotsMemorize <- plotGAM(copingModelManual, manualCVs, manualIVs, yLab="Memorization", 
                      xLab.predictors = "Openness", xLab.control = "Age")

copingModelManual <- gam(reuse ~ age*Openness, data=d, method="REML", family="binomial")
plotsReuse <- plotGAM(copingModelManual, manualCVs, manualIVs, yLab="Reuse", 
                         xLab.predictors = "Openness", xLab.control = "Age")

## unfortunately multiplot doesn't return, so use RStudio's export function.
multiplot(plotsPwm[[1]], plotsPwm[[2]], 
          plotsPaper[[1]], plotsPaper[[2]],
          plotsMemorize[[1]], plotsMemorize[[2]], 
          plotsReuse[[1]], plotsReuse[[2]], 
          cols=4)


## is age really negatively correlated with openness?
ageModel <- gam(Openness ~ age*it_background, data=d, method="REML")
summary(ageModel)
plotGAM(ageModel,list("age","Openness","it_background"), list("age","Openness","it_background"))

# correlation
correlationFrame <- d[,which(names(d) %in% list("age","it_background","Openness"))]
b5CorrPValues <- cor.mtest(correlationFrame)
corrplot(cor(correlationFrame),
         method="color",
         type="upper",
         addCoef.col = "black",
         p.mat = b5CorrPValues,
         sig.level = 0.01,
         insig = "blank"
)
# conclusion: not significantly in our dataset, but most people were younger. 
