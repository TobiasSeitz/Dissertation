# nudge efficacy.
library(ggplot2)
library(scales)
library(extrafont)

d = read.csv("./data/paper-graphs-data.csv",header=T,sep=";"); # final set of 83 participants
conditions <- c("Control","Passphrase", "Mangled", "Decoy");
d$group_id <- factor(d$group_sub, levels = c("A","BT","BD","C"), labels = conditions)
treatmentGroups <- c("Passphrase", "Mangled", "Decoy")

d.own.all <- d[d$used_own_password == "YES",]
d.own.treatment <- d[d$used_own_password == "YES" & d$group_id %in% treatmentGroups,]
d.treatment <- d[d$group_id %in% treatmentGroups,]


counts <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(counts) <- list("dummycol","behavior","count")



counts <- rbind(counts, data.frame(dummycol="y",
                                   behavior="ineffective (own password, weak score)",
                                   count=length(d.treatment$score[d.treatment$score < 3  & d.treatment$used_own_password == "YES"])))
counts <- rbind(counts, data.frame(dummycol="y",
                                   behavior="on par (own password, on par score)",
                                   count=length(d.treatment$score[d.treatment$score > 2 & d.treatment$used_own_password == "YES"])))
counts <- rbind(counts, data.frame(dummycol="y",
                                   behavior="effective (suggestion accepted)",
                                   count=length(d.treatment$score[d.treatment$used_own_password == "NO"])))


(proportionTreatmentEffectivenessPlot <- ggplot(data=counts, aes(x=dummycol,y=count,fill=behavior)) + 
  geom_bar(stat="identity",position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(y = "Proportion of Treatment Groups") +
  guides(fill = guide_legend(reverse=T, title="Nudge Efficacy")) +
  theme(legend.position="bottom") + 
  theme(axis.text.y = element_blank(),axis.title.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
    theme(text = element_text(family="Roboto"))
)


savePlot(proportionTreatmentEffectivenessPlot, "treatment-impact-v2.pdf",path="graphs", height=2.5)
