# analysis.
library(plyr)
library(reshape2)
library(stats)
library(ggplot2)
source('../util.R')

df <- read.csv('./emoji-full-data.csv', sep=';', header = T)

df.counts  <- read.csv('./emoji-selection-counts.csv', sep=';', header = T)
df.counts <- renameColumn(df.counts, "X...emoji","emoji")


# clean data.
# recode such that sentiments are 1=negative 5=positive
df$emoji.voluntary.usage <- 6 - df$emoji.voluntary.usage
df$emoji.requirement.annoying <- 6 - df$emoji.requirement.annoying
df$position_strategy <- ""
# where did the 1 emoji go?
df$position_strategy[
  df$position_emoji_1 == 1
] <- "1: First"
df$position_strategy[
  df$position_emoji_1 == df$pw_length
  ] <- "1: Last"

df$position_strategy[
  df$emojicount == 1 &
  df$position_emoji_1 > 1 &
  df$position_emoji_1 < df$pw_length
  ] <- "1: Middle"
# those who picked two emoji - where did they put them??
df$position_strategy[
  df$position_emoji_1 == 1 & 
    df$position_emoji_2 == 2
  ] <- "2: Both First"
df$position_strategy[
  df$position_emoji_1 == (df$pw_length - 1) & 
    df$position_emoji_2 == df$pw_length
  ] <- "2: Both Last"
df$position_strategy[
  df$position_emoji_1 == 1 & 
    df$position_emoji_2 == df$pw_length
  ] <- "2: First and Last"
df$position_strategy[
  df$position_emoji_1 > 1 & 
  df$position_emoji_1 < (df$pw_length-1) & 
    df$position_emoji_2 == df$pw_length
  ] <- "2: Middle and Last"
df$position_strategy[
  df$position_emoji_1 > 1 & 
    df$position_emoji_1 < df$pw_length & 
    df$position_emoji_2 > 2 &
    df$position_emoji_2 < (df$pw_length -1 )
  ] <- "2: Both Middle"

count(df$position_strategy)

df.sentiment.before <- data.frame(
  df$emoji.would.try, # If I had the option to add an emoji to my password, I might do this.
  df$emoji.would.easymemo # adding an emoji to my password would make it more memorable. 
); 

df.sentiment.after1 <- data.frame(
  df$sa_liked_emoji_pw # I found it good to use an emoji in the password,
); 
df.sentiment.after2 <- data.frame(
  df$emoji.convenient, # Using an emoji in my password was very convenient
  df$emoji.voluntary.usage, # !(I might have used an emoji even if I didn't have to.)
  df$emoji.requirement.annoying, # The additional requirement to add an emoji was annoying.
  df$emoji.pw.useful, # I find it useful to use emojis in passwords
  df$emoji.recall.easier, # It was easier to recall the emoji password than regular passwords
  df$emoji.future.use # I might consider using emojis in passwords in the future
)

df.sentiment.after2 <- df.sentiment.after2[1:39,] # remove no-shows

df.ux <- data.frame(
  df$ux_input_useful, # the way I entered the emoji made sense.
  df$ux_input_unsure, # i was made unsure by the option to enter it in two different ways
  df$ux_input_reduce # I'd find it better if there were only one way to enter it. 
)

# sentiment before the study
lapply(df.sentiment.before,summary)

# sentiment after completing first part
summary(df.sentiment.after1$df.sa_liked_emoji_pw)

# sentiment after completing first part
lapply(df.sentiment.after2,summary)
lapply(df.sentiment.after2,sd)


# compare before and after.
wilcox.test(df.sentiment.before$df.emoji.would.easymemo, 
            df.sentiment.after2$df.emoji.recall.easier,
            conf.int = T)
median(df$emoji.would.try)
median(df$emoji.voluntary.usage)

df.counts.long <- melt(df.counts, id.vars = c("emoji","utfEmoji","category"), measure.vars = c("controlCount","experimentalCount"))
df.counts.long$variable <- as.character(df.counts.long$variable)
df.counts.long$variable[df.counts.long$variable == 'controlCount'] <- "Control Group"
df.counts.long$variable[df.counts.long$variable == 'experimentalCount'] <- "Experimental Group"

### WHICH emoij was chosen and HOW OFTEN?
(distributionHistogram <- ggplot(df.counts.long, 
                                 aes(x=df.counts.long$emoji,y=value)) + 
                          geom_histogram(stat="identity") +
    scale_x_discrete(limits=df.counts$emoji) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
    labs(x="Emoji",y="Total Number of Occurances",fill="Study Group (Session 2)") +
    theme(axis.text.x = element_blank())
)
(distributionHistogramV2 <- ggplot(df.counts.long, 
                                 aes(x=df.counts.long$emoji,y=value,fill=category)) + 
    geom_histogram(stat="identity") +
    scale_x_discrete(limits=df.counts$emoji) + # hack: the data is sorted (desc) so we used that info to discretely scale the x-axis.
    labs(x="Emoji",y="Total Number of Occurances",fill="Study Group (Session 2)") +
    theme(axis.text.x = element_blank()) +
    theme(legend.position = "top")
)

savePlot(distributionHistogramV2, filename = "distribution-histogram-v2.pdf", path="graphs")

### emoji position

emojiPositions <- melt(df, measure.vars = c("position_emoji_1","position_emoji_2"), id.vars = c("X...participant","position_strategy"))
emojiPositions <- emojiPositions[emojiPositions$value != 0,]
emojiPositions$variable <- as.character(emojiPositions$variable)
emojiPositions$variable[emojiPositions$variable == 'position_emoji_1'] <- "First Emoji"
emojiPositions$variable[emojiPositions$variable == 'position_emoji_2'] <- "Second Emoji"

# histogram position count.
(positionHistogram <- ggplot(emojiPositions, aes(x=value,fill=variable)) + 
    geom_histogram(stat="count", binwidth = 1) +
    theme(legend.position = "top") +
    labs(x="Position of Emoji in Password",y="Number of Occurences",fill="Which Emoji")
)
# with strategy as fill
(positionHistogram <- ggplot(emojiPositions, aes(x=value,fill=position_strategy)) + 
    geom_histogram(stat="count", binwidth = 1) +
    scale_y_continuous(breaks = seq(0,10,2)) +
    scale_x_continuous(breaks = seq(1,19,1)) +
    theme(legend.position = "right") +
    labs(x="Position of Emoji in Password",y="Number of Occurences",fill="Emoji Selection Strategy")
)
savePlot(positionHistogram, filename = "position-histogram-by-strategy.pdf", path="graphs",height=3)

### emoji errors

