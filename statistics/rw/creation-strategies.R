library(ggplot2)

source("util.R")
csData <- read.table("rw/creation-strategies.csv",header=T,sep=";")

csData <- renameColumn(csData, "X...Techniques","Techniques")

labels <- c(
            "None of the above",
            "Other technique",
            "Password genrated by software",
            "Spelling words backwards",
            "Mnemonic phrase / acronym of a phrase",
            "Character substitutions",
            "Avoid dictionary words and names",
            "Symbols",
            "UPPER and lower case letters",
            "Letters and Digits"
            )

(
  strategyPlot <- ggplot(csData,aes(Techniques,Percentage)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(limits = labels) +
    geom_text(aes(y=Percentage + 2, label = paste0(Percentage,"%"), vjust=0.5)) +
    labs(x="Password Selection Techniques") +
    coord_flip() +
    theme(axis.text.y = element_text(size=13))
  )

savePlot(strategyPlot,"strategy-plot-kaspersky.pdf",width=12, height=3.5)
