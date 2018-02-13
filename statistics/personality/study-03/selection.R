
library(mgcv)
library(ggplot2)
library(reshape2)
library(psych)
library(corrplot)

source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

## makes a gam and ensures that "age" is smoothed as control variable, but the rest isn't.
getGam <- function(responseVar, d, predictors, controlVars, method=NULL,select=FALSE) {
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
  if(!is.null(method)){
    m <- gam(autoFormula, select = select, data=d, method=method); # adding method="REML" results in less magic.  
  }
  else{
    m <- gam(autoFormula, select = select, data=d); # adding method="REML" results in less magic.  
  }
  m
}

# this data looks stale: d <- read.csv("personality-pw-selection.csv", sep = ";", dec = ".")
# I've re-coded it:
d <- read.csv("data/data_recoded.csv", sep = ";", dec = ".")

# avoid that the the one third gender is factored into the models
d$gender[d$gender == 3] <- NA
d$gender <- factor(d$gender, levels = c(1,2), labels=c("Male","Female"))


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

dLongZxcvbn <- melt(d,id.vars="X...CASE",measure.vars=zxcvbnMetrics)
(pZxcvbnMetrics <- ggplot(dLongZxcvbn,aes(variable,value)) + 
    geom_boxplot(aes(colour=variable)) +
    labs(x="Metric",y="Value", col="Metric") +
    scale_y_continuous(breaks = seq(0,20,1)) +
    guides(colour = guide_legend(reverse=T)) +
    theme(axis.text.y = element_blank()) +
    coord_flip()
)
savePlot(pZxcvbnMetrics,"metrics-overview.pdf",path="graphs")

sink(file="descriptives.txt")
print(describe(d))
sink(file=NULL)
describe(d)


# correlation
correlationFrame <- d[,which(names(d) %in% predictorsB5)]
b5CorrPValues <- cor.mtest(correlationFrame)
corrplot(cor(correlationFrame),
         method="color",
         type="upper",
         addCoef.col = "black",
         p.mat = b5CorrPValues,
         sig.level = 0.01,
         insig = "blank"
         )




######################################################
#####
#####   selection models. 
#####
######################################################
controlVars <- list("age","gender","it_background","occupation")



predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");

autoModelsMetrics <- lapply(zxcvbnMetrics, getGam, d<-d, controlVars = controlVars, predictors = predictorsB5)
autoModelsMetrics_simple <- lapply(autoModelsMetrics, simplifyGAM)

# test summary
summary(autoModelsMetrics[[1]])
# test plot:
plot(autoModelsMetrics[[1]], jit=TRUE,pages=1)
plotGAM(autoModelsMetrics_simple[[1]], predictors = predictorsB5, controlVariables = controlVars)[[1]]

### plot 
lapply(autoModelsMetrics_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="zxcvbn-b5-predictors-", prefix.control="zxcvbn-b5-controls-",path="graphs/b5-reml",xLab.predictors = "Trait Scores")

for(i in autoModelsMetrics_simple){
  outputSummary(i,prefix="zxcvbn-",path="summaries")
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
b5Items <- subset(d, select = 18:38);
pcB5 <- princomp(b5Items) # omit ID variable
summary(pcB5)
plot(pcB5)

# pca plot showed 10 components (i.e. a lot more than the 5 we'd expect)
# factor analysis
b5FactorScores <- factanal(b5Items,factors=7,rotation="varimax",scores="regression")$scores
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




  
