source('../util.R')
source('../plotCI.R')


d = read.csv("./decoydata.csv",header=T,sep=";"); 
d$group <- factor(d$group);
group = c("group");

dvs <- c("length","digit","upper","lower","special","sequence","guesses","guesses_log10")

summaries <- lapply(dvs, function(var, data){
  summarySE(data, measurevar = var, groupvars = "group");
}, data<-d)

plots <- lapply(dvs, function(var, data, groupvar){
  plotCI(data, var, groupvar, xAxisTitle = var)
},data <- d, groupVariable <- "group")

lapply(seq_along(1:length(plots)), function(i){
  savePlot(plots[[i]], filename = paste("plot",i,".pdf"))
})

smry.length = summarySE(data,measurevar="length", groupvars=gvars);
smry.digit = summarySE(data,measurevar="digit",groupvars=gvars);
smry.guesses = summarySE(data,measurevar="guesses",groupvars=gvars);
smry.guesses_log10 = summarySE(data,measurevar="guesses_log10",groupvars=gvars);


plot <- ggplot(smry.length, aes(x=group, y=length, colour=group)) + 
  geom_errorbar(aes(ymin= length - ci, ymax= length +ci), width=.3) + 
  geom_line() + geom_point(size=4) + 
  xlab("Test Group") + ylab("Length (characters)") + 
  scale_colour_hue(name="Configuration",breaks=c("baseline", "target", "decoy","both"), labels=c("No suggestion", "Target suggested", "Decoy suggested", "Both suggested")) + 
  ggtitle("Effects of Password Suggestion on Password Length") + 
  theme(legend.justification=c(1,0),legend.position=c(1,0)) + 
  expand_limits(y=0);

print(smry.length);
print(smry.guesses_log10);
#plot <- ggplot(smry.guesses, aes(x=group, y=guesses, colour=group)) + geom_errorbar(aes(ymin= guesses - ci, ymax= guesses +ci), width=.3) + geom_line() + geom_point(size=4) + xlab("Test Group") + ylab("Guesses (estimated)") + scale_colour_hue(name="Configuration",breaks=c("baseline", "target", "decoy", "both"), labels=c("No suggestion", "Target suggested", "Decoy suggested", "Both suggested")) + ggtitle("Effects of Password Suggestion on Guesses Required") + theme(legend.justification=c(1,0),legend.position=c(1,0));
#plot <- ggplot(smry.guesses_log10, aes(x=group, y=guesses_log10, colour=group)) + geom_errorbar(aes(ymin= guesses_log10 - ci, ymax= guesses_log10 + ci), width=.3) + geom_line() + geom_point(size=4) + xlab("Test Group") + ylab("Guesses (estimated, Log10)") + scale_colour_hue(name="Configuration",breaks=c("baseline", "target", "decoy", "both"), labels=c("No suggestion", "Target suggested", "Decoy suggested", "Both suggested")) + ggtitle("Effects of Password Suggestion on Guesses Required") + theme(legend.justification=c(1,0),legend.position=c(1,0))+ expand_limits(y=0);
print(plot)



