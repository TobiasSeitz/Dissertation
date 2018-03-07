# my first guess number plot
library(scales); library(ggplot2)

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

makeGuessNumberSequence <- function(min = 0, cutOffMax = 20,step=0.5){
  seq(min, cutOffMax, step)
}

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

plotPercentageDataFrame <- function(df.p, labs.x = "Guess Number", labs.y = "Percentage Guessed", ...){
  # plot!
  ggplot(data=df.p, aes(x = guessNumber, y = percentCracked, colour=condition, group = condition)) + 
    geom_line() +
    scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
    scale_x_continuous(labels = math_format(10^.x)) +
    theme(legend.position = "top") + 
    labs(x=labs.x,y=labs.y, color="Condition")
}

plotGuessNumbers <- function(df, conditions, conditionColumn, guessNumberColumn, min = 0, cutOffMax = 20, ...){
  df.p <- getPercentageDataFrame(df=df,
                                 conditions=conditions,
                                 conditionColumn=conditionColumn,
                                 guessNumberColumn=guessNumberColumn, 
                                 min = min, 
                                 cutOffMax = cutOffMax, ...)
  plotPercentageDataFrame(df.p, ...);
  
}

## separate tests:
#plotGuessNumbers(df = d, 
#                 conditions = c("Control","Passphrase","Mangled","Decoy"),
#                 conditionColumn = "group_id",
#                 guessNumberColumn = "guesses_log10"
#                 )
#df.p <- getPercentageDataFrame(d, c("Control","Mangled"),conditionColumn = "group_id", "guesses_log10")
#plotPercentageDataFrame(df.p)