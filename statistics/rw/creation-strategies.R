library(ggplot2)

source("util.R")
csData <- read.table("rw/creation-strategies.csv",header=T,sep=";")

csData <- renameColumn(csData, "X...Techniques","Techniques")

labels <- c("Letters and Digits", 
            "UPPER and lower case letters",
            "Symbols",
            "Avoid dictionary words and names",
            "Character substitutions",
            "Mnemonic phrase / acronym of a phrase",
            "Spelling words backwards",
            "Password genrated by software",
            "Other technique",
            "None of the above"
            )

(
  strategyPlot <- ggplot(csData,aes(Techniques,Percentage)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y=Percentage + 2, label = paste0(Percentage,"%"), vjust=0.5)) +
    labs(x="Password Selection Techniques") +
    coord_flip() +
    theme(axis.text.y = element_text(size=13))
  )

savePlot(strategyPlot,"strategy-plot-kaspersky.pdf",width=12, height=3.5)
