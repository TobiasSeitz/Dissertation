### util

#library(extrafont)
library(mgcv)
library(ggplot2)
library(lm.beta)

# this can take a while, so we don't do that by default.
# font_import()
# fonts()
# fonttable()
# loadfonts()

renameColumn <- function(df,oldName,newName) {
  names(df)[names(df) == oldName] <- newName
}

# if we use a smoothening functin in a formula, we need to strip that from the column name later
# to get the original variable name. 
extractParameterFromSmoother <- function(x){gsub("\\)","",gsub("s\\(","",x))}

# This function helps avoiding smooth terms where linear modeling is possible.
# It checks a given model for estimated degrees of freedom (edf).
# If edf <= 1.1 (acknowledging rounding error), it uses that parameter as linear/parametric predictor
# If edf > 1.1 it is kept with the smooth function
# A new formula is derived and the GAM is calculated with that one
# example usage: lapply(listOfGAMs, simplifyGAM,d=myDataFrame);
#   smoothedModel: GAM object (from gam() of the mgcv package)
#   d: data frame to run the new gam() function on. 
# returns: gam object
simplifyGAM <- function(smoothedModel,select=FALSE,method="REML"){
  # copy the dataframe
  d <- model.frame(smoothedModel);
  
  mSummary <- summary(smoothedModel) # gives us everything we need to re-do the smoothedModel.
  # smoothed variables are the rownames of this table.
  mSmoothedFrame <- as.data.frame(mSummary$s.table);
  # parametric/linear variables are the rownames of this table
  mParametricFrame <- as.data.frame(mSummary$pTerms.table);
  
  # reponse variable / dependent 
  # attention: this currently fails if there is _MORE THAN ONE_ dependent variable.
  # we assume the first variable is the dependent.
  mResponse <- all.vars(smoothedModel$formula)[1]
  
  # read the correct row names of our frames.
  mParametricRows <- rownames(mParametricFrame)
  # here we need to ensure that we only take terms where smoothing does not improve the fit significantly.
  mLinearRows <- rownames(mSmoothedFrame[mSmoothedFrame[["edf"]] < 1.1,])
  # these will be kept with the smooth function, because their smoothing improves the fit.
  mCurveRows <- rownames(mSmoothedFrame[mSmoothedFrame[["edf"]] >= 1.1,])
  
  # some magic to retrieve the original variable names. 
  mLinearPredictors <- sapply(mLinearRows, extractParameterFromSmoother)
  mLinearPredictors <- unname(mLinearPredictors) # for some reason the name persists... #TODO
  
  # compile the right hand of the formula by merging the three vectors.
  nRightHand <- c(mLinearPredictors, mCurveRows, mParametricRows)
  
  # concatenate the right hand with a "+""
  nFormulaString <- paste(nRightHand, collapse=" + ")
  # concatenate the two pieces with the tilde
  nFormulaString <- paste(mResponse,nFormulaString,sep=" ~ ")
  # final step: assemble the formula
  nFormula = as.formula(nFormulaString)
  # and make the gam.
  m <- gam(nFormula,select = select, method=method,data=d)
  # optional, but recommended:
  # add a pointer to the data to the model
  # visreg, e.g., needs this to extract residuals.
  # m$data <- d
  m
}
smoothPredictors <- function(p,k=NULL){
  if(is.null(k)){
    p <- paste0("s(",p,")")  
  }
  else {
    p <- paste0("s(",p,",k=",k,")")  
  }
  p
}


# https://stackoverflow.com/a/30265548/1447479 
# creates a GAM with hardcoded control variables. Smoothes D_Age
getSmoothedGAM <- function(column,predictors,d,k=NULL,method=NULL,select=FALSE){
  # attention: to avoid weird collapses of the universe, make sure to have a continuous variable as first variable.
  # having D_Gender first breaks all kinds of things later.
  # you have been warned.
  controls <- "s(D_Age,k=5) + D_Gender + D_ComputerScienceBackground"
  smoothedPredictors <- lapply(predictors,function(p){
    if(is.null(k)){
      p <- paste0("s(",p,")")  
    }
    else {
      p <- paste0("s(",p,",k=",k,")")  
    }
    p
  })
  concatPredictors = paste(smoothedPredictors,collapse = "+")
  rightHand <- paste(concatPredictors, controls, sep = "+");
  autoFormula <- as.formula(paste(column,rightHand,sep = "~"))
  # see https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html
  if(!is.null(method)){
    m <- gam(autoFormula, select = select, data=d, method=method); # adding method="REML" results in less magic.  
  }
  else{
    m <- gam(autoFormula, select = select, data=d); # adding method="REML" results in less magic.  
  }
  m
}




## creates a directory if it does not exist, saves the plot and embeds the fonts (that's a TODO)
## defaults to PDF!
savePlot <- function(plot,filename,width=10,height=3,path=NULL){
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


## dependency on plotGAM.R!
generatePDF <- function(model, 
                        controlVariables,
                        predictors,
                        prefix.predictors = "model-predictors-", 
                        prefix.control = "model-controls-", 
                        path = "graphs", 
                        xLab.predictors = NULL){
  dependent <- all.vars(model$formula)[1]
  autoPlots <- plotGAM(model,controlVariables=controlVariables, predictors=predictors, yLab = dependent, xLab.predictors=xLab.predictors) 
  savePlot(autoPlots[[1]],paste0(prefix.predictors,dependent,".pdf"),path=path)
  savePlot(autoPlots[[2]],paste0(prefix.control,dependent,".pdf"),path=path)
}

# creates a text file with the summary.
outputSummary <- function(m,prefix="",path=NULL){
  # create a directory if it does not exist
  if(!is.null(path)) dir.create(path, showWarnings = FALSE)
  else path <- getwd()
  dependent <- all.vars(m$formula)[1]
  filename <- paste0(prefix,dependent,".txt")
  fullpath <- file.path(path,filename);
  sink(fullpath);
  
  print(summary(m))
  print("\nBETAs:")
  print(lm.beta(m))
  print("\nGAM.CHECK:")
  print(gam.check(m))
  #print("\nAUTOCORRELATION")
  #print(acf(residuals(m)))
  #print(pacf(residuals(m)))
  #print("\nCONCURVITY")
  #print(concurvity(m,full=TRUE))
  sink(file=NULL)
}

