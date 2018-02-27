# selection 2 - this time with aline's final data set

library(mgcv)
library(ggplot2)
library(reshape2)
library(psych)
library(corrplot)
library(compare)

source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

## makes a gam and ensures that "age" is smoothed as control variable, but the rest isn't.
getGam <- function(responseVar, d, predictors, controlVars, select=FALSE,family="gaussian",...) {
  smoothedPredictors <- lapply(predictors,smoothPredictors,k=5)
  smoothedControls <- lapply(controlVars, function(var){
    if (var=="age"){
      c <- smoothPredictors(var,k=5)
    } else{
      c <- as.character(var)
    }
    c
  })
  
  concatPredictors = paste(smoothedPredictors,collapse = "+")
  concatControls = paste(smoothedControls,collapse = "+")
  rightHand <- paste(concatPredictors, concatControls, sep = "+");
  autoFormula <- as.formula(paste(responseVar,rightHand,sep = "~"))
  # see https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html
  m <- gam(autoFormula, select = select, data=d, family=family, ...); # adding method="REML" results in less magic.  
  m
}

# this data looks stale: d <- read.csv("personality-pw-selection.csv", sep = ";", dec = ".")
# I've re-coded it:
da <- read.csv("data/Neumann_Aline_Datensatz.csv", sep = ";", dec = ".",na.strings = "#N/A")


# housekeeping

# factorization
da$gender <- factor(da$gender,labels=c("Male","Female"))
da$it <- factor(da$it,labels=c("no it","it"))
da$occupation <- factor(da$occupation)

# remove outlier in row 177 (length==40)
da <- da[-c(177),]
# rename columns properly.
da <- renameColumn(da,"o","Openness")
da <- renameColumn(da,"c","Conscientiousness")
da <- renameColumn(da,"e","Extraversion")
da <- renameColumn(da,"a","Agreeableness")
da <- renameColumn(da,"n","Neuroticism")


######################################################
#####
#####   Descriptives and stuff.
#####
######################################################
zxcvbnMetrics <- list("length",
                      "digits","symbols","uppercase","lowercase",
                      #"guesses",
                      "score",
                      "guesses_log10",
                      "substitutions","chunks"
);

dLongZxcvbn <- melt(da,id.vars="X...CASE",measure.vars=zxcvbnMetrics)
(pZxcvbnMetrics <- ggplot(dLongZxcvbn,aes(variable,value)) + 
    geom_boxplot(aes(colour=variable)) +
    labs(x="Metric",y="Value", col="Metric") +
    scale_y_continuous(breaks = seq(0,40,1)) +
    guides(colour = guide_legend(reverse=T)) +
    theme(axis.text.y = element_blank()) +
    coord_flip()
)
savePlot(pZxcvbnMetrics,"metrics-v2-overview.pdf",path="graphs",width=9,height=3)

sink(file="descriptives.txt")
print(describe(da))
sink(file=NULL)
describe(da)


# correlation
predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");
controlVars <- list("age","gender","it","occupation")

correlationFrame <- da[,which(names(da) %in% predictorsB5)]
b5CorrPValues <- cor.mtest(correlationFrame)
corrplot(cor(correlationFrame),
         method="color",
         type="upper",
         addCoef.col = "black",
         p.mat = b5CorrPValues,
         sig.level = 0.01,
         insig = "blank"
)

# do IT people create longer passwords?
wilcox.test(length~it,data = da, alternative="greater")

######################################################
#####
#####   selection models. 
#####
######################################################

autoModelsMetrics <- lapply(zxcvbnMetrics, getGam, d<-da, controlVars = controlVars, predictors = predictorsB5)
autoModelsMetrics_simple <- lapply(autoModelsMetrics, simplifyGAM)


### there was a weird part in the data that made me doubt that aline remembered to re-encode reversely keyed items,
### so I did this and ran the models againt. 
#autoModelsMetricsKeyed <- lapply(zxcvbnMetrics, getGam, d<-dKeyed, controlVars = controlVars, predictors = predictorsB5Keyed)
#autoModelsMetricsKeyed_simple <- lapply(autoModelsMetricsKeyed, simplifyGAM)


# test summary
summary(autoModelsMetrics[[1]])
summary(autoModelsMetrics_simple[[1]])
summary(autoModelsMetricsKeyed[[1]])
summary(autoModelsMetricsKeyed_simple[[1]])
anova(autoModelsMetrics_simple[[1]],autoModelsMetricsKeyed_simple[[1]])

# test plot:
plot(autoModelsMetrics[[1]], jit=TRUE,pages=1)
plotGAM(autoModelsMetrics_simple[[1]], predictors = predictorsB5, controlVariables = controlVars)[[1]]
plotGAM(autoModelsMetricsKeyed_simple[[1]], predictors = predictorsB5, controlVariables = controlVars)[[1]]

### plot 
lapply(autoModelsMetrics_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="predictors-", prefix.control="zxcvbn-b5-controls-",path="graphs/b5-zxcvbn",xLab.predictors = "Trait Scores")
lapply(autoModelsMetricsKeyed_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5Keyed,
       prefix.predictors="predictors-", prefix.control="zxcvbn-b5k-controls-",path="graphs/b5-zxcvbn",xLab.predictors = "Trait Scores")



## summaries
for(i in autoModelsMetrics_simple){
  outputSummary(i,prefix="zxcvbn-v2-",path="summaries/zxcvbn")
}
for(i in autoModelsMetricsKeyed_simple){
  outputSummary(i,prefix="zxcvbn-keyed-",path="summaries")
}


## correlation plot
ggplot(data=d, aes(x=Neuroticism,y=length)) + 
  geom_point() +
  #geom_jitter() +
  geom_smooth(method="lm")


######################################################
#####
#####   Factor Extraction
#####
######################################################


# principal components
# derive components
b5Items <- subset(da, select = 18:38);

# internal consistency
#alpha(subset(d,select=18:21),check.keys=TRUE) # extraversion
alpha(b5Items,check.keys=TRUE)

# it looks as though all neuroticism items should be reversed. could be done with something like this 
# (right now that won't work but the idea is to subtract the score from 6 to reverse it. 
# d[subset(d,select=30:33)] <- 6 - subset(d,select=30:33)

alpha(subset(d,select=30:33),check.keys=TRUE) # neuroticism
alpha(subset(b5Items,select=13:16),check.keys=TRUE) # neuroticism

#let's try something.
testReverseItems <- list("E1R","E3R","A1R","A3R","A4R","C2R","N2R","O5R")
lapply(testReverseItems,function(item){
  b5Items[[item]] <- 6 - b5Items[[item]]
})

pcB5 <- princomp(b5Items) # omit ID variable
summary(pcB5)
plot(pcB5)

# pca plot showed 10 components (i.e. a lot more than the 5 we'd expect)
# factor analysis

b5FactorScores <- factanal(b5Items,factors=7,scores="regression")$scores
b5FactorScores <- as.data.frame(b5FactorScores)

faB5 <- data.frame(d,b5FactorScores)
predictorsFA <- as.list(names(b5FactorScores))


autoModelsMetricsFA <- lapply(zxcvbnMetrics, getGam,d<-faB5, predictors=predictorsFA,
                              controlVars = controlVars)

autoModelsMetricsFA_simple <- lapply(autoModelsMetricsFA,simplifyGAM);


### plot 
lapply(autoModelsMetricsFA_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsFA,
       prefix.predictors="zxcvbn-fa-predictors-", prefix.control="zxcvbn-fa-controls-",path="graphs/fa",xLab.predictors = "Factor Scores")
# summaries
for(i in autoModelsMetricsFA_simple){
  outputSummary(i,prefix="zxcvbn-",path="summaries/fa")
}


######################################################
#####
#####   Influence / Selection Strategies 
#####
######################################################

influenceMetrics <- list(
  "Influence_commercials","Influence_music","Influence_experience",
  "Influence_family","Influence_literature","Influence_music",
  "Influence_mnemonics", "Influence_residency", "Influence_TV",
  "Influence_videogames"
)
autoModelsInfluence <- lapply(influenceMetrics, getGam, d<-d, controlVars = controlVars, predictors = predictorsB5, family="binomial")
autoModelsInfluence_simple <- lapply(autoModelsInfluence, simplifyGAM, family="binomial")
lapply(autoModelsInfluence_simple,summary)
