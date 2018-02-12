
library(mgcv)
library(ggplot2)
library(reshape2)

source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");

d <- read.csv("personality-pw-selection.csv", sep = ";", dec = ".")

# avoid that the the one third gender is factored into the models
d$gender[d$gender == 3] <- NA
d$gender <- factor(d$gender, levels = c(1,2), labels=c("Male","Female"))

pwSummary <- summary(d)

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



######################################################
#####
#####   selection models. 
#####
######################################################
controlVars <- list("age","gender","it_background")

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

autoModelsMetrics <- lapply(zxcvbnMetrics, getGam, d<-d, controlVars = controlVars, predictors = predictorsB5)
autoModelsMetrics_simple <- lapply(autoModelsMetrics, simplifyGAM)

# test summary
summary(autoModelsMetrics[[1]])
# test plot:
plot(autoModelsMetrics[[1]], jit=TRUE,pages=1)
plotGAM(autoModelsMetrics[[1]], predictors = predictorsB5, controlVariables = controlVars)[[2]]

### plot 
lapply(autoModelsMetrics_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="zxcvbn-b5-predictors-", prefix.control="zxcvbn-b5-controls-",path="graphs/b5-reml",xLab.predictors = "Trait Scores")

for(i in autoModelsMetrics_simple){
  outputSummary(i,prefix="zxcvbn-",path="summaries")
}

predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");

predictorsModel <- lapply(predictorsB5, smoothPredictors,k=5)



responseVariables <- list("length")

testFormula <- paste("length",paste0(predictorsModel,controlVars,collapse="+"),sep="~")

testModel <- gam(as.formula(testFormula),data=d,method="REML")
testModel_simple <- simplifyGAM(testModel)
summary(testModel_simple)

plot(testModel_simple, pages=1,jit=T)
# gender 1 = male, 2 = femaled
