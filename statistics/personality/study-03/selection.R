library(mgcv)
library(ggplot2)
library(reshape2)
library(psych)
library(corrplot)
library(compare)

source("../../plotCI.R");
source("../../plotGAM.R");
source("../../util.R");
source("getGam.R");


# this data looks stale: d <- read.csv("personality-pw-selection.csv", sep = ";", dec = ".")
# I've re-coded it:
d <- read.csv("data/data_recoded.csv", sep = ";", dec = ".")

# avoid that the the one third gender is factored into the models
d$gender[d$gender == 3] <- NA
d$gender <- factor(d$gender, levels = c(1,2), labels=c("Male","Female"))

## try and reverse neuroticism
#d$Neuroticism_r <- (6-d$N1) + (6-d$N2R) + (6-d$N3) + (6-d$N4)

b5Items <- subset(d, select = 18:38);

# subjective complexity factor.
# lowercase perceived complexity (PC): 1
# uppercase PC: 2
# digit PC: 3
# symbols PC: 4
d$complexity_level <- 0
d$complexity_level <- d$lowercase + (2*d$uppercase) + (3*d$digits) + (4*d$symbols)

# the scores are actually wrong as of now:
# we need to reverse all b5 items.
for(item in names(b5Items)) {
  d[[item]] <- 6 - d[[item]]
}
# now recalculate the scores:
d$Openness <- d$O1 + d$O2 + d$O3 + d$O4 + d$O5R;
d$Conscientiousness <- d$C1 + d$C2R + d$C3 + d$C4;
d$Extraversion <- d$E1R + d$E2 + d$E3R + d$E4;
d$Agreeableness <- d$A1R + d$A2 + d$A3R + d$A4R;
d$Neuroticism <- d$N1 + d$N2R + d$N3 + d$N4;
# and that's it!


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
predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");
controlVars <- list("age","gender","it_background","occupation")

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

# do IT people create longer passwords?
wilcox.test(length~it_background,data = d,alternative = "less")

# were men more likely to work in IT?
cor(d[,which(names(d) %in% list("gender","it_background","age"))])

######################################################
#####
#####   selection models. 
#####
######################################################


autoModelsMetrics <- lapply(zxcvbnMetrics, getGam, d<-d, controlVars = controlVars, predictors = predictorsB5)
autoModelsMetrics_simple <- lapply(autoModelsMetrics, simplifyGAM)


# test summary
summary(autoModelsMetrics[[1]])
summary(autoModelsMetrics_simple[[1]])
anova(autoModelsMetrics_simple[[1]],autoModelsMetrics[[1]], test="Chisq")

# test plot:
plot(autoModelsMetrics[[1]], jit=TRUE,pages=1)
plotGAM(autoModelsMetrics_simple[[1]], predictors = predictorsB5, controlVariables = controlVars)[[1]]


### plot 
lapply(autoModelsMetrics_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="zxcvbn-b5-predictors-", prefix.control="zxcvbn-b5-controls-",path="graphs/b5-reml",xLab.predictors = "Trait Scores")

## summaries
for(i in autoModelsMetrics_simple){
  outputSummary(i,prefix="zxcvbn-",path="summaries")
}


## correlation plot
ggplot(data=d, aes(x=Neuroticism,y=length)) + 
  geom_point() +
  #geom_jitter() +
  geom_smooth(method="loess")
  
  
######################################################
#####
#####   Factor Extraction
#####
######################################################


# principal components
# derive components

# internal consistency
#alpha(subset(d,select=18:21),check.keys=TRUE) # extraversion
alpha(b5Items,check.keys=F)

# it looks as though all neuroticism items should be reversed. could be done with something like this 
# (right now that won't work but the idea is to subtract the score from 6 to reverse it. 
# d[subset(d,select=30:33)] <- 6 - subset(d,select=30:33)

alpha(subset(d,select=30:33),check.keys=TRUE) # neuroticism
alpha(subset(b5Items,select=13:16),check.keys=TRUE) # neuroticism

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
## only "residency" with good explanation of deviance (R-sq: 0.113, dev expl: 42.2%)

######################################################
#####
#####   PERCEIVED COMPLEXITY
#####
######################################################
perceivedComplexityModel <- getGAM("complexity_level",predictorsB5, controlVars, d<-d, controls.smoothed = c("age"))
perceivedComplexityModel_simple <- simplifyGAM(perceivedComplexityModel)
generatePDF(perceivedComplexityModel_simple, controlVariables =  controlVars, predictors = predictorsB5)
## okay, nothing. 

