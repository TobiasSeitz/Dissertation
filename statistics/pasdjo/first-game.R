## load important packages.
library("ggplot2")
library("plyr")
library(extrafont)
library(pairwiseCI)


## font handling
font_install("fontcm")
loadfonts()
fonttable()
fonts()

## error bars / confidence intervals: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/ 
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



###########
##
##
## main analysis starts here
##
##
###########
## game in the long form --> condition-value as value in row, not a column for each condition.
first_game_v1_long <- read.table("../data/pasdjo-v1-longform-firstgame.csv", header=TRUE, 
                                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)
first_game_v2_long <- read.table("../data/pasdjo-v2-longform-firstgame.csv", header=TRUE, 
                                 sep=",", na.strings="NA", dec=".", strip.white=TRUE)


## put your conditions here
conditions = c("common","mangled","passphrase","random")
conditions_v2 = c("common","mangled","passphrase","random","predictable")
conditions_labels = c("Common","Mangled","Passphrase","Random")
conditions_labels_v2 = c("Common","Mangled","Passphrase","Random", "Predictable")

## alias conditions as "independent variables" (ivs);
ivs = conditions;
ivs_labels = conditions_labels;

# apply the labels on the dataset 
# for our dataset, this is not really necessary, but when we need more 
# explanation as to what the variables mean, this is a good way to do it. 
first_game_v1_long$condition = factor(first_game_v1_long$condition,levels=ivs,labels=ivs_labels)
first_game_v2_long$condition = factor(first_game_v2_long$condition,levels=conditions_v2,labels=conditions_labels_v2)

# first plot absolute user Ratings.
rm(userRatingSummary)
dv <- "userRating"
iv_column <- "condition"

userRatingSummary <- summarySE(first_game_v1_long, measurevar=dv, groupvars=iv_column);
scoreSummary <- summarySE(first_game_v1_long, measurevar="score", groupvars=iv_column);
userRatingSummary_v2 <- summarySE(first_game_v2_long, measurevar = "userRating", groupvars = iv_column)
scoreSummary_v2 <- summarySE(first_game_v2_long, measurevar="score", groupvars=iv_column);

#### v1 (one year data)
(g <- ggplot(userRatingSummary, aes(x=userRating, y=ivs, fill=condition)) + 
  geom_errorbarh(aes(x=userRating, xmin=userRating-ci, xmax=userRating+ci, height=0)) +
  geom_point(aes(colour=condition), size=5) + 
  geom_point(aes(x=score,y=ivs),scoreSummary,size=5) +
  scale_y_discrete(labels=ivs_labels) +
  scale_x_continuous("\n Average user ratings and 95% CIs",limits=c(1,5),breaks=seq(1, 5, 0.5)) +
  theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
  theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_line(colour = "grey95")) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size=18)) +
  theme(axis.text.y = element_text(colour = "black", size = 16)) +
  theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none"))

g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="user-rating-v1-ci.pdf", path="../graphs", width=11, height=4.0)
embed_fonts("../graphs/user-rating-v1-ci.pdf", outfile="../graphs/user-rating-v1-ci.pdf")
g

#### v2
(g <- ggplot(userRatingSummary_v2, aes(x=userRating, y=conditions_v2, fill=condition)) + 
    geom_errorbarh(aes(x=userRating, xmin=userRating-ci, xmax=userRating+ci, height=0)) +
    geom_point(aes(colour=condition), size=5) + 
    geom_point(aes(x=score,y=conditions_v2),scoreSummary_v2,size=5) +
    scale_y_discrete(labels=conditions_labels_v2) +
    scale_x_continuous("\n Average user ratings and 95% CIs",limits=c(1,5),breaks=seq(1, 5, 0.5)) +
    theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
    theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_line(colour = "grey95")) +
    theme(panel.grid.minor = element_blank()) +
    theme(text = element_text(size=18)) +
    theme(axis.text.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
    theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position = "none"))

g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="user-rating-v2-ci.pdf", path="../graphs", width=11, height=4.0)
embed_fonts("../graphs/user-rating-v2-ci.pdf", outfile="../graphs/user-rating-v2-ci.pdf")
g



## visualize the differences.

scoreSummary <- summarySE(first_game_v1_long, measurevar="score", groupvars=iv_column);

scoreRatingSummary <- merge(userRatingSummary, scoreSummary, by="condition");
# now viz!
g <- ggplot(scoreRatingSummary, aes(x=score, y=ivs, fill=condition))+
  geom_errorbarh(aes(x=score, xmin=score-ci, xmax=score+ci, height=0)) +
  geom_point(size=5) + 
  scale_y_discrete(labels=ivs_labels) +
  scale_x_continuous("\n Average scores and 95% CIs",limits=c(1,5),breaks=seq(1, 5, 0.5)) +
  theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
  theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_line(colour = "grey95")) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size=18)) +
  theme(axis.text.y = element_text(colour = "black", size = 16)) +
  theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none")

g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="score-ci.pdf", path="../graphs", width=11, height=4.0)
embed_fonts("../graphs/score-ci.pdf", outfile="../graphs/score-ci.pdf")
g




### tendencies / deviations

# delete dirty data
rm(tendency)
# now create a standard-error/confidence interval for all conditions on dependent variable
dv <- "tendency"
iv_column <- "condition"

tendency_v1 <- summarySE(first_game_v1_long, measurevar=dv, groupvars=iv_column);
tendency_v2 <- summarySE(first_game_v2_long, measurevar=dv, groupvars=iv_column);

# plot the tendencies as CI plot.
g <- ggplot(tendency_v1, aes(x=tendency, y=ivs, fill=condition)) +
  geom_errorbarh(aes(x=tendency, xmin=tendency-ci, xmax=tendency+ci, height=0)) +
  scale_colour_brewer(palette = "Paired") +
  scale_y_discrete(labels=ivs_labels) +
  geom_point(aes(colour=condition), size=4) +
  scale_x_continuous("\n Mean deviation from accurate rating and 95% CIs",limits=c(-2,1),breaks=seq(-2, 1, 0.5)) +
  theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
  theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size=18)) +
  theme(axis.text.y = element_text(colour = "black", size = 16)) +
  theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none")

g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="deviation-ci.pdf", path="graphs", width=11, height=2.0)
embed_fonts("graphs/deviation-ci.pdf", outfile="graphs/deviation-ci.pdf")
g


###########
##
##
## Part 2: compare conditions with hypothesis tests.
##
##
###########

# non-parametric tests only

## omnibus test
omnibus_v1 <- friedman.test(tendency ~ condition | user, data=first_game_v1_long)
omnibus_v2 <- friedman.test(tendency ~ condition | user, data=first_game_v2_long)
omnibus_v1
omnibus_v2

## post hoc

# calculate p-values of pairwise differences:
pairs.tests <- pairwise.wilcox.test(first_game_v1_long$tendency, 
                                    first_game_v1_long$condition, 
                                    p.adj = "bonf",
                                    na.rm = TRUE,
                                    paired = TRUE,
                                    conf.int = TRUE,
                                    conf.level = 0.95);

# extract p-values
sigs <- pairs.tests[[3]];

###
## confidence intervals
###

# Median.diff to use non-parametric version.
pairs <- pairwiseCI(tendency~condition, 
                    data=first_game_v1_long, 
                    alternative ="two.sided", 
                    conf.level=0.95, 
                    method="Median.diff");

print(pairs)

pairFrame <- as.data.frame(pairs[[1]])

# this is a bit nasty. we use the p-values from the pairwise.wilcox.test and add them
# to the dataFrame that has the respective confidence intervals.
# this allows us to color significant results differently.
# the order is top to bottom, left to right.
pairFrame$pVals <- c(sigs[1,1],sigs[2,1],sigs[3,1],sigs[2,2],sigs[3,2],sigs[3,3])


# set an alpha level
alpha <- 0.05

# bonferroni correction, rounded to three decimal digits.
correctedAlpha <- round(alpha / nrow(pairFrame), 3); ## the last number is the df

# where do we cut significant / non-significant results?
cuts <- cut(pairFrame$pVals,c(1,correctedAlpha,0),label=c(paste("p < ", correctedAlpha),"Non-Sig"));

g <- ggplot(pairFrame, aes(x=estimate,y=compnames, colour=cuts)) +
  geom_vline(xintercept=0, color="grey55", linetype=2, size=1.001) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, height=0)) +
  geom_point(size=4)  +
  scale_x_continuous("",limits=c(-2.5,1.5),breaks=seq(-2.5, 1.5, 0.5)) +
  # scale_y_discrete("Comparisons") +
  theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
  theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  theme(axis.text.x = element_text(colour = "grey60", size = 20)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  labs(subtitle = "Estimated Median Difference of Deviation and 95% CI")
g
g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="deviation-difference-v1.pdf", path="../graphs", width=11, height=3.0)
embed_fonts("../graphs/deviation-difference-v1.pdf", outfile="../graphs/deviation-difference-v1.pdf")

######
######
###### version 2 post-hocs
######
######

# calculate p-values of pairwise differences:
pairs.tests <- pairwise.wilcox.test(first_game_v2_long$tendency, 
                                    first_game_v2_long$condition, 
                                    p.adj = "bonf",
                                    na.rm = TRUE,
                                    paired = TRUE,
                                    conf.int = TRUE,
                                    conf.level = 0.95);

# extract p-values
sigs <- pairs.tests[[3]];

###
## confidence intervals
###

# Median.diff to use non-parametric version.
pairs <- pairwiseCI(tendency~condition, 
                    data=first_game_v1_long, 
                    alternative ="two.sided", 
                    conf.level=0.95, 
                    method="Median.diff");

print(pairs)

pairFrame <- as.data.frame(pairs[[1]])

# this is a bit nasty. we use the p-values from the pairwise.wilcox.test and add them
# to the dataFrame that has the respective confidence intervals.
# this allows us to color significant results differently.
# the order is top to bottom, left to right.
pairFrame$pVals <- c(sigs[1,1],sigs[2,1],sigs[3,1],sigs[2,2],sigs[3,2],sigs[3,3])


# set an alpha level
alpha <- 0.05

# bonferroni correction, rounded to three decimal digits.
correctedAlpha <- round(alpha / nrow(pairFrame), 3); ## the last number is the df

# where do we cut significant / non-significant results?
cuts <- cut(pairFrame$pVals,c(1,correctedAlpha,0),label=c(paste("p < ", correctedAlpha),"Non-Sig"));

g <- ggplot(pairFrame, aes(x=estimate,y=compnames, colour=cuts)) +
  geom_vline(xintercept=0, color="grey55", linetype=2, size=1.001) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, height=0)) +
  geom_point(size=4)  +
  scale_x_continuous("",limits=c(-2.5,1.5),breaks=seq(-2.5, 1.5, 0.5)) +
  # scale_y_discrete("Comparisons") +
  theme(panel.background = element_rect(fill = 'grey98', linetype = "solid"), legend.position="bottom") +
  theme(panel.grid.major = element_line(colour = "grey87"), panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  theme(axis.text.x = element_text(colour = "grey60", size = 20)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  labs(subtitle = "Estimated Median Difference of Deviation and 95% CI")
g
g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="deviation-difference-v1.pdf", path="../graphs", width=11, height=3.0)
embed_fonts("../graphs/deviation-difference-v1.pdf", outfile="../graphs/deviation-difference-v1.pdf")