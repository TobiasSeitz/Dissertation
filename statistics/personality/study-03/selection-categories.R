# selection - categories
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
d$type <- factor(d$type)

d$t_common_topo <- 0
d$t_hardened_topo <- 0
d$t_mnemonic <- 0
d$t_passphrase <- 0
d$t_random <- 0
d$t_simple <- 0
d$t_systematic <- 0

d$t_common_topo[d$type=='common-topo'] <- 1
d$t_hardened_topo[d$type=='hardened-topo'] <- 1
d$t_mnemonic[d$type=='mnemonic'] <- 1
d$t_passphrase[d$type=='passphrase'] <- 1
d$t_random[d$type=='random'] <- 1
d$t_simple[d$type=='simple'] <- 1
d$t_systematic[d$type=='systematic'] <- 1

predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");
controlVars <- list("age","gender","it_background")

binaryPasswordTypeVariables <- list("t_common_topo","t_hardened_topo","t_mnemonic","t_passphrase","t_random","t_simple","t_systematic")
autoTypeModels <- lapply(binaryPasswordTypeVariables, getGam, d<-d, 
          controlVars <- controlVars,
          predictors = predictorsB5,
          k=8,
          family="binomial")
autoTypeModels_simple <- lapply(autoTypeModels, simplifyGAM, family="binomial", k=8)
# plot
lapply(autoTypeModels_simple, generatePDF, 
       controlVariables = controlVars, predictors = predictorsB5,
       prefix.predictors="pw-type-predictors-", prefix.control="pw-type-controls-",path="graphs/pw-type",xLab.predictors = "Trait Scores")
for(i in autoTypeModels_simple){
  outputSummary(i,prefix="pw-type-",path="summaries/pw-type")
}

