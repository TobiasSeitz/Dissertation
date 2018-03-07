# diss analysis re-do
#install.packages("outliers")
library(outliers)
library(pairwiseCI)

source('../util.R')
source('../plotCI.R')
source('../plotGAM.R')
source('../guess-number-plot.R')

d = read.csv("./data/paper-graphs-data.csv",header=T,sep=";"); # final set of 83 participants
d99 <- read.csv("./decoydata.csv",header=T,sep=";")

conditions <- c("Control","Passphrase", "Mangled", "Decoy");
d$group_id <- factor(d$group_sub, levels = c("A","BT","BD","C"), labels = conditions);
d99$group_id <- factor(d99$group_id,levels = c(1,2,3,4), labels = conditions)

treatmentGroups <- c("Passphrase", "Mangled", "Decoy")
zxcvbnMetrics <- c("password_length","digit","upper","lower")

for(x in zxcvbnMetrics){
  d[[x]] <- rm.outlier(d[[x]], fill=TRUE)
  d99[[x]] <- rm.outlier(d99[[x]], fill=TRUE)
}

# who used their own password?
d.own.all <- d[d$used_own_password == "YES",]
d99.own.all <- d99[d99$used_own_password == 1,]
# who used their own password despite suggestions?
d.own.treatment <- d[d$used_own_password == "YES" & d$group_id %in% treatmentGroups,]
d99.own.treatment <- d99[d99$used_own_password == 1 & d99$group_id %in% treatmentGroups,]

#count(d99_own$score[d99_own$group_id == "Passphrase"])
#count(d99_own$score[d99_own$group_id == "Mangled"])
#count(d99_own$score[d99_own$group_id == "Decoy"])

#zxcvbnMetrics <- c("length","digit","upper","lower","special","sequence","guesses","guesses_log10", "leet")

## remove outliers
lapply(zxcvbnMetrics, function(x){
  d[[x]] <- rm.outlier(d[[x]])
})

kwstats <- lapply(zxcvbnMetrics, function(metric, data){
  newDat <- data;
  newDat[[metric]] <- rm.outlier(newDat[[metric]],fill = TRUE, median = TRUE);
  
  f <- as.formula(paste0(metric,"~","group_id"))
  kruskal.test(formula = f, data=newDat)
}, data = d)


lapply(zxcvbnMetrics, function(metric, data){
  data[[metric]] <- rm.outlier(data[[metric]], fill = TRUE, median = TRUE);
  p <- plotCI(data,
              dependentVariable = metric, 
              groupVariable = "group_id", 
              xAxisTitle = metric)
  savePlot(p, filename = paste0(metric,".pdf"),path = "graphs")
}, data = d_own)


(ciGuessesAll <- plotCI(d, dependentVariable = "guesses_log10", groupVariable = "group_id", xAxisTitle = "Guesses (log10)", minValue = 1, maxValue =17, step=2))
savePlot(ciGuessesAll, "graphs/ci-guessesLog10-all.pdf", width=5, height=3)
(ciGuessesOwn <- plotCI(d.own.all, dependentVariable = "guesses_log10", groupVariable = "group_id", xAxisTitle = "Guesses (log10)", minValue = 1, maxValue = 17, step=2))
savePlot(ciGuessesOwn, "graphs/ci-guessesLog10-own.pdf", width=5, height=3)

(ciLengthOwn <- plotCI(d.own.all, dependentVariable = "password_length", groupVariable = "group_id", xAxisTitle = "Password Length", minValue = 8, maxValue = 16, step=1))
savePlot(ciLengthOwn, "graphs/ci-length-own.pdf", width=10, height=3)

kruskal.test(guesses_log10 ~ group_id, d) 
summary(lm(guesses_log10 ~ group_id, d))

lapply(kwstats, print)


### plot guess numbers
(guessNumberPlot <- plotGuessNumbers(d.own.all, conditions = conditions, conditionColumn = "group_id", guessNumberColumn = "guesses_log10"))
savePlot(guessNumberPlot, "guess-number-plot.pdf",path="graphs", width=6, height=3)

# GAM the hell out of the guess numbers.
percentageFrame <- getPercentageDataFrame(d.own.all, conditions = conditions, conditionColumn = "group_id", guessNumberColumn = "guesses_log10")
percentageGAM <- gam(percentCracked ~ s(guessNumber) +condition,data=percentageFrame)
generatePDF(percentageGAM, controlVariables = c("guessNumber"), predictors = c("condition"))
outputSummary(percentageGAM)

summary(gam(pw_length_corrected ~ group_id, data = d.own.all))
