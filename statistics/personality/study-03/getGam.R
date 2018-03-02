
## makes a gam and ensures that "age" is smoothed as control variable, but the rest isn't.
getGam <- function(responseVar, d, predictors, controlVars, select=FALSE,family="gaussian",k=5,...) {
  smoothedPredictors <- lapply(predictors,smoothPredictors,k=k)
  smoothedControls <- lapply(controlVars, function(var){
    if (var=="age"){
      c <- smoothPredictors(var,k=k)
    } else{
      c <- as.character(var)
    }
    c
  })
  
  concatPredictors = paste(smoothedPredictors,collapse = "+")
  concatControls = paste(smoothedControls,collapse = "+")
  rightHand <- paste(concatPredictors, concatControls, sep = "+");
  autoFormula <- as.formula(paste(responseVar,rightHand,sep = "~"))
  # see https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html
  m <- gam(autoFormula, select = select, data=d, family=family, ...); # adding method="REML" results in less magic.  
  m
}
