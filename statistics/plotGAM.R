### plot GAM
### author: Tobi Seitz - tobi@tobitobi.de
### (c) 2018

library(ggplot2)
library(visreg)
library(plyr)
library(extrafont)

# this can take a while, so we don't do that by default.
# font_import()
# fonts()
# fonttable()
# loadfonts()


## avoids ugly x-axis breaks (only prints n pretty ticks)
## stolen from: Axeman's answer on Stack Overflow https://stackoverflow.com/a/44886993/1447479
int_breaks <-
  function(x, n = 5)
    pretty(x, n)[pretty(x, n) %% 1 == 0]

# creates two separate plots of GAMs 
#   model: created by gam()
#   controlVariables: vector/list of the names of the control variables
#   predictors: vector/list of the names of the predictors
#   yLab: custom y-axis label
#   xLab.control: custom x-axis label for the control variables plot
#   xLab.predictors: custom x-axis label for the predictors plot
plotGAM <-
  function(model,
           controlVariables,
           predictors,
           yLab = NULL,
           xLab.control = NULL,
           xLab.predictors = NULL,
           plotResiduals = TRUE) {
    # point size for the residuals
    pointSize <- 1
    residualColor <- '#aaaaaa' # gray
    
    #
    # most of the following was stolen from Dag Hjerman's answer on Stack Overflow: https://stackoverflow.com/a/21182922/1447479
    #
    
    # use plot = FALSE to get plot data from visreg without plotting
    plotData <- visreg(model, type = "contrast", plot = FALSE)
    # The output from visreg is a list of the same length as the number of 'x' variables,
    #   so we use ldply to pick the objects we want from the each list part and make a dataframe:
    smooths <- ldply(plotData, function(part)
      data.frame(
        Variable = part$meta$x,
        x = part$fit[[part$meta$x]],
        smooth = part$fit$visregFit,
        lower = part$fit$visregLwr,
        upper = part$fit$visregUpr
      ))
    residuals <- ldply(plotData, function(part)
      data.frame(
        Variable = part$meta$x,
        x = part$res[[part$meta$x]],
        y = part$res$visregRes
      ))
    
    #### subset datafor control variables and predictors, to put them into separate figures in a manuscript
    # shout out to: https://stackoverflow.com/a/11612314/1447479
    controlSmooths <- smooths[smooths$Variable %in% controlVariables, ]
    predictorSmooths <- smooths[smooths$Variable %in% predictors, ]
    controlResiduals <-
      residuals[residuals$Variable %in% controlVariables, ]
    predictorResiduals <-
      residuals[residuals$Variable %in% predictors, ]
    
    # basic plot setup with the smooths data
    predictorPlot <- ggplot(predictorSmooths, aes(x, smooth))
    controlPlot <- ggplot(controlSmooths, aes(x, smooth))
    
    # residuals can get very visually heavy, so it sometimes is a good idea to hide them. 
    # this is done through the plotResiduals flag
    if (plotResiduals) {
      predictorPlot <-
        predictorPlot + geom_point(data = predictorResiduals,
                                   aes(x, y),
                                   col = residualColor,
                                   size = pointSize) + theme(legend.position = "none")
      controlPlot <-
        controlPlot + geom_point(data = controlResiduals,
                                 aes(x, y),
                                 col = residualColor,
                                 size = pointSize) + theme(legend.position = "none")
    }
    
    # make sure the important part sits on top by plotting the lines after the points.
    predictorPlot <- predictorPlot + 
      # the estimated curve
      geom_line() +
      # lower bound curve
      geom_line(aes(y = lower), linetype = "dashed") +
      # upper bound curve
      geom_line(aes(y = upper), linetype = "dashed") +
      # we only want integer breaks as x-axis ticks 
      scale_x_continuous(breaks = int_breaks) +
      # create a new plot for each level in $Variable
      # each plot can have its own x-axis which is achieved with free_x
      facet_grid(. ~ Variable, scales = "free_x") +
      labs(x = "Predictor Score")
    
    controlPlot <- controlPlot + geom_line() +
      geom_line(aes(y = lower), linetype = "dashed") +
      geom_line(aes(y = upper), linetype = "dashed") +
      scale_x_continuous(breaks = int_breaks) +
      facet_grid(. ~ Variable, scales = "free_x") +
      labs(x = "Control Variable Level")
    
    ## the user can set a couple of custom labels.
    if (!is.null(yLab)) {
      predictorPlot <- predictorPlot + labs(y = yLab)
      controlPlot <- controlPlot + labs(y = yLab)
    }
    if (!is.null(xLab.control)) {
      controlPlot <- controlPlot + labs(x = xLab.control)
    }
    if (!is.null(xLab.predictors)) {
      predictorPlot <- predictorPlot + labs(x = xLab.predictors)
    }
    
    ## return a list of the plots. 
    list(predictorPlot, controlPlot)
  }