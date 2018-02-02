library(ggplot2)
library(visreg)

int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 


plotGAM <- function(model, positionColumn,yLab=NULL) {
  # use plot = FALSE to get plot data from visreg without plotting
  plotData <- visreg(model, type = "contrast", plot = FALSE)
  # The output from visreg is a list of the same length as the number of 'x' variables,
  #   so we use ldply to pick the objects we want from the each list part and make a dataframe: 
  smooths <- ldply(plotData, function(part)   
    data.frame(Variable = part$meta$x, 
               x=part$fit[[part$meta$x]], 
               smooth=part$fit$visregFit, 
               lower=part$fit$visregLwr, 
               upper=part$fit$visregUpr))
  #### subset datafor control variables
  controlSmooths <- smooths[
    which(smooths$Variable == 'Age' |
            smooths$Variable == 'Gender' |
            smooths$Variable == 'IT' |
            smooths$Variable == positionColumn),];
  bigfiveSmooths <- smooths[
    which(smooths$Variable == 'Extraversion' |
            smooths$Variable == 'Agreeableness' |
            smooths$Variable == 'Conscientiousness' |
            smooths$Variable == 'Neuroticism' |
            smooths$Variable == 'Openness'
    ),];
  
  ## make sure the facets will have a good title afterwards.
  levels(controlSmooths$Variable)[levels(controlSmooths$Variable) == positionColumn] <- paste("Task Number ","(",positionColumn,")", sep="")
  
  bigfivePlot <- ggplot(bigfiveSmooths, aes(x, smooth)) + geom_line() +
    geom_line(aes(y=lower), linetype="dashed") + 
    geom_line(aes(y=upper), linetype="dashed") + 
    scale_x_continuous(breaks=int_breaks)+
    facet_grid(. ~ Variable, scales = "free_x") +
    labs(x="Trait Score")
  
  controlPlot <- ggplot(controlSmooths, aes(x, smooth)) + geom_line() +
    geom_line(aes(y=lower), linetype="dashed") + 
    geom_line(aes(y=upper), linetype="dashed") + 
    scale_x_continuous(breaks=int_breaks)+
    facet_grid(. ~ Variable, scales = "free_x") +
    labs(x="Control Variable Level")
  
  if(!is.null(yLab)) {
    bigfivePlot <- bigfivePlot + labs(y=yLab)
    controlPlot <- controlPlot + labs(y=yLab)
  }
  
  list(bigfivePlot,controlPlot)
}
