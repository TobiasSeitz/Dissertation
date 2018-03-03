# inverse

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

d <- read.csv("data/data_recoded.csv", sep = ";", dec = ".")

b5Variables <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");

zxcvbnMetricsInverse <- list("digits",
                      "symbols",
                      "uppercase",
                      "lowercase",
                      "substitutions",
                      "chunks"
);

controlVars <- list("age","gender","it_background")

b5Models <- lapply(b5Variables, getGam, predictors=zxcvbnMetricsInverse, controlVars = controlVars, d <- d,k=4)
b5Models_simple <- lapply(b5Models, simplifyGAM)

for(i in b5Models_simple) {
  outputSummary(i,prefix="",path="summaries/inverse")
}
