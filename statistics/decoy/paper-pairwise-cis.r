### Prolific Analysis.
library(RcmdrMisc)
library(ggplot2);
library(stats);

if(! exists("decoy_prolific")) {
print("need data");
decoy_prolific <- read.csv("./data/paper-graphs-data.csv",header=T,sep=";"); 
} else{
	print("got data");
}



decoy_prolific_individual <- subset(decoy_prolific, usedPW == "own");

usedData <- decoy_prolific;

aov.length <- aov(data=usedData, password_length~group_sub);
aov.digits <- aov(data=usedData, digit~group_sub);
aov.guesses_log10 <- aov(data=usedData, guesses_log10~group_sub);
aov.special <- aov(data=usedData, special~group_sub);
aov.upper <- aov(data=usedData, upper~group_sub);
aov.lower <- aov(data=usedData, lower~group_sub);
aov.score <- aov(data=usedData, score~group_sub);


print(anova(aov.length));
print(anova(aov.digits));
print(anova(aov.guesses_log10));
print(anova(aov.special));
print(anova(aov.upper));
print(anova(aov.lower));
print(anova(aov.score));

# Tukey Tests
tky.length <- as.data.frame(TukeyHSD(aov.length)$group_sub);
tky.digits <- as.data.frame(TukeyHSD(aov.digits)$group_sub);
tky.guesses_log10 <-as.data.frame(TukeyHSD(aov.guesses_log10)$group_sub);
tky.special <- as.data.frame(TukeyHSD(aov.special)$group_sub);
tky.upper <- as.data.frame(TukeyHSD(aov.upper)$group_sub);
tky.lower <- as.data.frame(TukeyHSD(aov.lower)$group_sub);
tky.score <- as.data.frame(TukeyHSD(aov.score)$group_sub);

# Pairwise comparison.
pairs <- rownames(tky.length)
tky.length$pair <- pairs;
tky.digits$pair <- pairs;
tky.guesses_log10$pair <- pairs;
tky.special$pair <- pairs;
tky.upper$pair <- pairs;
tky.lower$pair <- pairs;
tky.score$pair <- pairs;



## CI Plots ##
myAes <- aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), label=c("p<0.01","p<0.05","Non-Sig")));
myAesNonSig <- aes();
lines <- geom_hline(yintercept=0, lty="11", colour="grey30", size=1);

if(identical(usedData,decoy_prolific_individual)) {
	suffix = "_individual.pdf";
} else {
	suffix = ".pdf";
}


### Password Length ###
ggplot(tky.length, myAesNonSig) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) + geom_point(aes(pair, diff),size=4) + xlab("Pair") + ylab("Character count. Delta of arithmetic mean") + labs(colour="") + theme(legend.justification=c(1,1),legend.position=c(1,1)) + coord_flip();
ggsave(file = paste("ci-length",suffix,sep=""),  width = 6, height = 3);

### Digits
ggplot(tky.digits, myAes) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) +  geom_point(aes(pair, diff),size=4) +  xlab("Pair") + ylab("Delta of arithmetic mean") + labs(colour="") + coord_flip();
ggsave(file = paste("ci-digits",suffix,sep=""),  width = 6, height = 3);

### guesses log 10
ggplot(tky.guesses_log10, myAes) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) +  geom_point(aes(pair, diff),size=4) + xlab("Pair") + ylab("Mean difference (log10)") + labs(colour="") + theme(legend.justification=c(1,1),legend.position=c(1,1)) + coord_flip();
ggsave(file = paste("ci-guesseslog10",suffix,sep=""),  width = 6, height = 3);

### uppercase letters
ggplot(tky.upper, myAes) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) +  geom_point(aes(pair, diff),size=4) +  xlab("Pair") + ylab("Delta of arithmetic mean") + labs(colour="") + coord_flip();
ggsave(file = paste("ci-uppercase",suffix,sep=""),  width = 6, height = 3);

ggplot(tky.lower, myAes) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) +  geom_point(aes(pair, diff),size=4) +  xlab("Pair") + ylab("Delta of arithmetic mean") + labs(colour="") + coord_flip();
ggsave(file = paste("ci-lowercase",suffix,sep=""),  width = 6, height = 3);

ggplot(tky.special, myAes) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) +  geom_point(aes(pair, diff),size=4) +  xlab("Pair") + ylab("Delta of arithmetic mean") + labs(colour="") + coord_flip();
ggsave(file = paste("ci-special",suffix,sep=""),  width = 6, height = 3);

ggplot(tky.score, myAes) + lines + geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1.5) +  geom_point(aes(pair, diff),size=4) +  xlab("Pair") + ylab("Delta of arithmetic mean") + labs(colour="") + coord_flip();
ggsave(file = paste("ci-score",suffix,sep=""),  width = 6, height = 3)


#readline("Press <Enter> to continue");

