# selection

d <- read.csv("emoji-selection-counts.csv",sep=";")

names(d)[names(d) %in% c("X...emoji")] <- "Emoji"



chisq.test(d$count)
fisher.test(d$count)
