library(extrafont)
library(plyr)
library(ggplot2)

library(extrafont)

# this can take a while!
# font_import()
# fonts()
# fonttable()
loadfonts()

## error bars / confidence intervals: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/ 
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


plotCI <- function(longDF, dependentVariable, groupVariable, xAxisTitle=NULL, yLabels=NULL, minValue=NULL,maxValue=NULL, step=1) {
  ## sanity checks
  if (is.null(longDF[[dependentVariable]]) | is.null(longDF[[groupVariable]])){
    return("Data frame does not contain the dependent or group variable")
  }
  
  ## dependecy: summarySE function
  difficultySummary <- summarySE(longDF, measurevar=dependentVariable, groupvars=groupVariable);
  
  ## by default we want to avoid the "lie-factor" and take the min/max values from the dataset to communicate the relation of CIs
  if(is.null(maxValue)) maxValue <- max(longDF[[dependentVariable]],na.rm=TRUE)
  if(is.null(minValue)) minValue <- min(longDF[[dependentVariable]],na.rm=TRUE)
  
  ## 
  g <- ggplot(difficultySummary, aes(x=difficultySummary[[dependentVariable]], y=difficultySummary[[groupVariable]], fill=difficultySummary[[groupVariable]])) +
    geom_errorbarh(aes(x=difficultySummary[[dependentVariable]], xmin=difficultySummary[[dependentVariable]]-ci, xmax=difficultySummary[[dependentVariable]]+ci, height=0)) +
    geom_point(aes(colour=difficultySummary[[groupVariable]]), size=5) + 
    theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
    theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_line(colour = "grey95")) +
    theme(panel.grid.minor = element_blank()) +
    theme(text = element_text(size=18)) +
    theme(axis.text.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
    theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position = "none");
  if (!is.null(xAxisTitle)){
    g <- g + scale_x_continuous(name=xAxisTitle,limits=c(minValue,maxValue),breaks=seq(minValue, maxValue, step))
  }
  if(!is.null(yLabels)){
    g <- g + scale_y_discrete(labels=yLabels)
  }
  g
}

## creates a directory if it does not exist, saves the plot and embeds the fonts (that's a TODO)
## defaults to PDF!
savePlot <- function(plot,filename,width,height,path=NULL){
  # create a proper path
  if(!is.null(path)) dir.create(path, showWarnings = FALSE)
  else path <- getwd()
  fullpath <- file.path(path,filename);
  
  # check if the the file name ends in .pdf 
  grepped <- grep("\\.pdf$",filename,ignore.case=TRUE)
  if (length(grepped) == 0) {
    # let's append the .pdf so that ggsave knows what to do.
    filename <- paste(filename,".pdf",sep="")
  }
  
  ggsave(plot=plot, filename=filename, path=path, width=width, height=height)
  embed_fonts(file=fullpath, outfile=fullpath)
  paste("Saved plot to",fullpath)
}