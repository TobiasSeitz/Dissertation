### util

renameColumn <- function(df,oldName,newName) {
  names(df)[names(df) == oldName] <- newName
}