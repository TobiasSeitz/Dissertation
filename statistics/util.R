### util

library(extrafont)

# this can take a while, so we don't do that by default.
# font_import()
# fonts()
# fonttable()
# loadfonts()

renameColumn <- function(df,oldName,newName) {
  names(df)[names(df) == oldName] <- newName
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