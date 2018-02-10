# tests
testSimplifiedModel <- simplifyModel(autoModelsRatingB5[[1]])
plotGAM(updatedModelsB5[[1]],controlVariables=controlVariables, predictors=predictorsB5, xLab.predictors="Trait Score")[[2]] 
