# minimal working example
library(mgcv)
library(ggplot2)
library(plyr)
library(visreg)

df <- read.csv("https://raw.githubusercontent.com/TobiasSeitz/r-swag/master/testdata/dataset-N100.csv", sep = ";", dec = ".")
sub <- df[,(names(df) %in% c("D_Gender","G_Overall","B5_Openness","B5_Neuroticism"))]

### factorize things
sub$D_Gender <- factor(df$D_Gender,levels=c("Male","Female"),labels=c("Male","Female"))

predictors <- list("B5_Neuroticism", "B5_Openness")

formula1 <- "G_Overall ~ s(B5_Openness) + s(B5_Neuroticism) + D_Gender";
formula2 <- "G_Overall ~ D_Gender + s(B5_Openness) + s(B5_Neuroticism)";

myPlot <- function(gaModel,predictors){
  # next two statements from https://stackoverflow.com/a/21182922/1447479
  plotData <- visreg(gaModel, type = "contrast", plot = FALSE)
  #######
  ##
  ##  HERE'S A PROBLEM -- obviously caused by the ldply call - but why?
  ##  in formula1, where D_Gender is last, "smooth" contains the right factor levels for D_Gender --> (1,2)
  ##  in formula2 that begins with D_Gender, the factor levels for D_Gender are --> ("Male","Female") -- why?
  ## 
  smooths <- ldply(plotData, function(part)
    data.frame(
      Variable = part$meta$x,
      x = part$fit[[part$meta$x]],
      smooth = part$fit$visregFit,
      lower = part$fit$visregLwr,
      upper = part$fit$visregUpr
    ))
  ### do not plot the dichotomous control variable "D_Gender"
  predictorSmooths <- smooths[smooths$Variable %in% predictors, ]
  (predictorPlot <- ggplot(predictorSmooths, aes(x, smooth)) + 
    geom_line() +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_line(aes(y = upper), linetype = "dashed") +
    facet_grid(. ~ Variable, scales = "free_x"))
    
  predictorPlot
}

m1 <- gam(as.formula(formula1),data=sub)
summary(m1) ### all good
myPlot(m1, predictors) ### all good
m2 <- gam(as.formula(formula2),data=sub) ### all good
summary(m2) ### all good, no obvious change

### Problem: 
myPlot(m2, predictors)  
### Error: "Each group consists of only one observation. Do you need to adjust the group aesthetic?"
