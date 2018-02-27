# memorability

library(mgcv)
library(ggplot2)
library(reshape2)

source("../../plotGAM.R");
source("../../util.R");

# attention: the scores are off (reversed) in this dataset - 
# look at selection.R to reverse the items and re-calculate the scores.
d <- read.csv("data/data_recoded.csv", sep = ";", dec = ".")

# avoid that the the one third gender is factored into the models
d$gender[d$gender == 3] <- NA
d$gender <- factor(d$gender, levels = c(1,2), labels=c("Male","Female"))

predictorsB5 <- list("Openness","Conscientiousness", "Extraversion", "Agreeableness","Neuroticism");

forgettingModelSplit <- gam(forgot ~ lowercase + digits + symbols + uppercase + chunks + 
                         s(Openness,k=5) + s(Agreeableness,k=5) + s(Extraversion,k=5) + s(Conscientiousness,k=5) + s(Neuroticism,k=5),
                       data=d, method="REML", family="binomial")
summary(forgettingModelSplit)

forgettingModelLength <- gam(forgot ~ length + 
                               s(Openness,k=5) + s(Agreeableness,k=5) + s(Extraversion,k=5) + s(Conscientiousness,k=5) + s(Neuroticism,k=5),
                             data=d, method="REML", family="binomial")

summary(forgettingModelLength)

forgettingModelOnlyLength <- gam(forgot ~ length,
                             data=d, method="REML", family="binomial")

summary(forgettingModelOnlyLength)


outputSummary(forgettingModelSplit, "all-", "summaries/memorability")
plotGAM(forgettingModelSplit, controlVariables = list("lowercase","digits","symbols","uppercase","chunks"), 
        predictors = predictorsB5
          )
outputSummary(forgettingModelLength, "length-", "summaries/memorability")
plotGAM(forgettingModelLength, controlVariables = list("length"), 
        predictors = predictorsB5
)

outputSummary(forgettingModelOnlyLength, "only-length-", "summaries/memorability")
anova.gam(forgettingModelLength,forgettingModelOnlyLength, test="Chisq", freq = T)
