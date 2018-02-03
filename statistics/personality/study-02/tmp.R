### STANDALONE STRENGTH RATING
library(mgcv)


# read data from 100, cleaned dataset, preprocessed from excel.
d100 <- read.csv("dataset-N100.csv", sep = ";", dec = ".")

### factorize things
d100$D_Gender <- factor(d100$D_Gender,levels=c("Male","Female"),labels=c("Male","Female"))
d100$D_ComputerScienceBackground <- factor(d100$D_ComputerScienceBackground, levels=c("No","Yes"), labels=c("No","Yes"))

responseVariablesRating <- c("G_Common","G_Passphrase","G_Pattern",
                             "G_LengthLong","G_LengthShort",
                             "G_Weak","G_Medium","G_Strong",
                             "G_Digits","G_Special","G_Uppercase")

controlVariables <- list("D_Age","D_Gender","D_ComputerScienceBackground")
predictorsB5 <- list("B5_Extraversion","B5_Agreeableness", "B5_Conscientiousness", "B5_Neuroticism", "B5_Openness")
predictorsSeBIS <- list("SeBIS_Securement","SeBIS_Awareness", "SeBIS_Updating", "SeBIS_Passwords")
predictorsGDMS <- list("GDMS_Rational","GDMS_Intuitive", "GDMS_Avoidant",  "GDMS_Dependent", "GDMS_Spontaneous")


autoModelsRating <- lapply(responseVariablesRating,function(column){
  autoFormula <- as.formula(paste(column,"~ s(D_Age) + D_Gender + D_ComputerScienceBackground + s(B5_Extraversion) + s(B5_Agreeableness) +
                              s(B5_Conscientiousness) + s(B5_Neuroticism) + s(B5_Openness)"))
  gam(autoFormula, data=d100);
})


extractParameterFromSmoother <- function(x){
  firstBraces <- gsub("s\\(","",x)  
  endBraces <- gsub("\\)","",firstBraces)
  endBraces
}
updatedFormulae <- lapply(autoModelsRating, function(model){
  mSummary <- summary(model) # gives us everything we need.
  mSmoothedFrame <- as.data.frame(mSummary$s.table);
  mParametricFrame <- as.data.frame(mSummary$pTerms.table);
  mResponse <- all.vars(model$formula)[1]
  mLinearRows <- rownames(mSmoothedFrame[mSmoothedFrame$edf <= 1.1,])
  mParametricRows <- rownames(mParametricFrame)
  mCurveRows <- rownames(mSmoothedFrame[mSmoothedFrame$edf > 1.1,])
  mLinearPredictors <- sapply(mLinearRows, extractParameterFromSmoother)
  mLinearPredictors <- unname(mLinearPredictors) # for some reason the name persists...
  
  nRightHand <- c(mParametricRows, mLinearPredictors,mCurveRows)
  
  nFormulaString <- paste(nRightHand, collapse=" + ")
  nFormulaString <- paste(mResponse,nFormulaString,sep=" ~ ")
  print(nFormulaString)
  nFormula = as.formula(nFormulaString)
  nFormula
})
