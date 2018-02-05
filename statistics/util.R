### util

#library(extrafont)
library(mgcv)
library(ggplot2)

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
simplifyGAM <- function(smoothedModel){
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
  # here we need to ensure that we only take the predictors that show only one degree of freedom.
  mLinearRows <- rownames(mSmoothedFrame[mSmoothedFrame$edf <= 1.1,])
  # these will be kept with the smooth function, because the edf are greater
  mCurveRows <- rownames(mSmoothedFrame[mSmoothedFrame$edf > 1.1,])
  
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
  m <- gam(nFormula,data=d)
  # optional, but recommended:
  # add a pointer to the data to the model
  # visreg, e.g., needs this to extract residuals.
  # m$data <- d
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