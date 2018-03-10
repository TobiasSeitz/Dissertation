# errors details
library(plyr)
library(reshape2)
#library(stats)
library(ggplot2)
source('../util.R')


df.errors <- read.csv('./emoji-errors-failed-logins.csv', sep=';', header = T)
df.errors.details <- read.csv('./emoji-errors-details.csv', sep=';', header = T)
df.errors <- renameColumn(df.errors, "X...condition","group")
df.errors.details <- renameColumn(df.errors.details, "condition","group")
df.errors.details <- renameColumn(df.errors.details, "X...totalemoji","totalemoji")

overview <- function(){
  df.errors$group <- as.character(df.errors$group)
  df.errors$group[df.errors$group=='apple'] <- "Control Group"
  df.errors$group[df.errors$group=='google'] <- "Experimental Group"
  df.errors$group <- factor(df.errors$group);
  errorEmojiTicks <- c(
    "cherry_blossom",
    "cyclone",
    "bird",
    "tea",
    "revolving_hearts",
    "diamond",
    "camel",
    "penguin",
    "white_check_mark",
    "desktop_computer",
    "smile",
    "sweat_smile",
    "blush",
    "headphones",
    "poultry_leg"
  )
  ( # by emoji
    errorHistogram1 <- ggplot(df.errors, aes(x=emoji,y=errors,label=participant,fill=category)) + 
      geom_histogram(stat="identity",colour="white") +
      geom_text(size = 3, position = position_stack(vjust = 0.5), color="white") +
      scale_x_discrete(limits=errorEmojiTicks) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
      scale_y_continuous(breaks=seq(0,6,1)) +
      facet_grid(group ~ .) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #theme(legend.position = "top") +
      labs(x="Emoji",y="Errors during log-in",fill="Emoji Category",color="Participant")
  )
  
  ( # by group
    errorHistogram2 <- ggplot(df.errors, aes(x=category,y=errors,color=emoji)) + 
      geom_histogram(stat="identity") +
      #geom_text( size = 2, position = position_stack(vjust = 0.5), color="white") +
      #scale_x_discrete(limits=errorEmojiTicks) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
      scale_y_continuous(breaks=seq(0,11,1)) +
      facet_grid(group ~ .) +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "top") +
      labs(x="Emoji",y="Errors during log-in",color="Emoji")
  )
  
  ( # by emoji & participant
    errorHistogram3 <- ggplot(df.errors, aes(x=emoji,y=errors,color=category,label=participant)) + 
      geom_histogram(stat="identity") +
      geom_text(size = 2, position = position_stack(vjust = 0.5), color="white") +
      scale_x_discrete(limits=errorEmojiTicks) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
      scale_y_continuous(breaks=seq(0,6,1)) +
      facet_grid(group ~ .) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #theme(legend.position = "top") +
      labs(x="Emoji",y="Errors during log-in",fill="Emoji Category",color="Category")
  )
  
  ( # by group
    errorHistogram4 <- ggplot(df.errors, aes(x=dummy,y=errors,label=emoji)) + 
      geom_histogram(stat="identity", color="white") +
      geom_text( size = 2, position = position_stack(vjust = 0.5), color="white") +
      #scale_x_discrete(limits=errorEmojiTicks) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
      scale_y_continuous(breaks=seq(0,11,1)) +
      facet_grid(group ~ category) +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "top") +
      theme(axis.text.x = element_blank()) +
      labs(x="Category",y="Errors during log-in",color="Emoji")
  )
  
  
  savePlot(errorHistogram1, "error-by-emoji-1.pdf",height=5,path="graphs")
  savePlot(errorHistogram2, "error-by-group-1.pdf",height=5,path="graphs")
  savePlot(errorHistogram3, "error-by-emoji-2.pdf",height=5,path="graphs")
  savePlot(errorHistogram4, "error-by-group-5.pdf",height=5,path="graphs")
}

details <- function(){
  df.errors.details$group <- as.character(df.errors.details$group)
  df.errors.details$group[df.errors.details$group=='apple'] <- "Control Group"
  df.errors.details$group[df.errors.details$group=='google'] <- "Experimental Group"
  df.errors.details$group <- factor(df.errors.details$group)
  df.errors.details$totalerrors <- df.errors.details$totalemoji * df.errors.details$errors
  df.errors.details$errors <- (-1) * df.errors.details$errors;
  df.errors.details$totalerrors <- (-1) * df.errors.details$totalerrors
  
  ( # by group
    detailHistogram1 <- ggplot(df.errors.details, aes(x=dummy,y=errors,label=emoji,fill=emoji)) + 
      #geom_bar(data = post, stat = "identity") +
      geom_bar(stat = "identity",color="white") +
      geom_hline(yintercept=0,color="black") +
      geom_text( size = 2, position = position_stack(vjust = 0.5), color="white") +
      #scale_x_discrete(limits=errorEmojiTicks) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
      #scale_y_continuous(breaks=seq(0,11,1)) +
      facet_grid(group ~ category) +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_blank()) +
      labs(x="Category",y="Login Succuesses (positive) vs. Login Failures (negative)",color="Emoji")
  )
  
  savePlot(detailHistogram1, "detailed-by-emoji-1.pdf",height=5,path="graphs")
}

