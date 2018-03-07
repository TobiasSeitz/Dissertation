library(scales); # for 10^x
library(ggplot2)

## calculates the percentage of cracked passwords for a given cut-off threshold
#   guessNumberLog: double - cut off threshold in log-scale.
#   cGuessNumbers: vector of doubles - password guess numbers in a certain data set
#   return: percentage * 100.
getPercentageCracked <- function(guessNumberLog, cGuessNumbers){
  cracked = 0;
  for(x in cGuessNumbers) {
    if (x < guessNumberLog){
      cracked = cracked + 1;
    }
  }
  percentage = (cracked / length(cGuessNumbers)) * 100;
  percentage;
}

# generates an equidistant number sequence for guess numbers 
# essentially just a wrapper around the seq() function with named parameters.
#   min: starting point for guess number sequence
#   cutOffMax: maximum value for the sequence, i.e. the cut-off threshold for guess number plots.
makeGuessNumberSequence <- function(min = 0, cutOffMax = 20,step=0.5){
  seq(min, cutOffMax, step)
}

# prepares a dataframe that can be easily visualized as guessability plot 
#   df: dataframe (long form)
#   conditions: vector of condition names (used for comparing guessability)
#   conditionColumn: name of the column bearing the condition values (group column)
#   guessNumberColumn: name of the column that holds the vector of guess numbers.
#   returns: data frame with three columns: guessNumber, condition, percentCracked.
getPercentageDataFrame <- function(df, conditions, conditionColumn, guessNumberColumn, ...){
  #TODO error handling / santity checking of all required parameters.
  t <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(t) <- list("guessNumber","condition","percentCracked")
  guessNumberSequence <- makeGuessNumberSequence(...);
  for(gN in guessNumberSequence){
    for (condition in conditions){
      conditionSubset <- df[df[,conditionColumn] == condition,]
      cGuessNumbers <- conditionSubset[[guessNumberColumn]]
      t <- rbind(t, data.frame(guessNumber=gN, 
                               condition=condition, 
                               percentCracked = getPercentageCracked(gN, cGuessNumbers)))
    }
  }
  t
}

# plots a dataframe.
#   df.p: data frame that needs to have 3 colums "guessNumber", "percentCracked", and "condition".
#   labs.x: Custom X Axis label
#   labs.y: Custom Y Axis label
#   ...: currently not used
#   returns: ggplot object
plotPercentageDataFrame <- function(df.p, labs.x = "Guess Number", labs.y = "Percentage Guessed", ...){
  # plot!
  ggplot(data=df.p, aes(x = guessNumber, y = percentCracked, colour=condition, group = condition)) + 
    geom_line() +
    scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
    scale_x_continuous(labels = math_format(10^.x)) +
    theme(legend.position = "top") + 
    labs(x=labs.x,y=labs.y, color="Condition")
}

# creates a guessability plot for a given data set. Does all the data transformations according to the specified columsn.
#   df: data frame (long form)
#   conditions: vector of condition names (used for comparing guessability)
#   conditionColumn: name of the column holding the condition values (group column)
#   guessNumberColumn: name of the column that holds the vector of guess numbers.
#   min: starting point (guess number) for the plot on the x axis
#   cutOffMax: cut off threshold for the plot. 
#   ...: passed to getPercentageDataFrame and plotPercentageDataFrame.
#   returns: ggplot object.
plotGuessNumbers <- function(df, conditions, conditionColumn, guessNumberColumn, min = 0, cutOffMax = 20, ...){
  df.p <- getPercentageDataFrame(df=df,
                                 conditions=conditions,
                                 conditionColumn=conditionColumn,
                                 guessNumberColumn=guessNumberColumn, 
                                 min = min, 
                                 cutOffMax = cutOffMax, ...)
  
  plotPercentageDataFrame(df.p, ...)
}

## tests:
#plotGuessNumbers(df = d, 
#                 conditions = c("Control","Passphrase","Mangled","Decoy"),
#                 conditionColumn = "group_id",
#                 guessNumberColumn = "guesses_log10"
#                 )
#df.p <- getPercentageDataFrame(d, c("Control","Mangled"),conditionColumn = "group_id", "guesses_log10")
#plotPercentageDataFrame(df.p)