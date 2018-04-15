## score distributions
library(ggplot2)

source("../util.R")

v1LongForm <- read.table("data/dataset-v1/rounds.csv", header=TRUE, 
                         sep=",", na.strings="NA", dec=".", strip.white=TRUE)

v2LongForm <- read.table("data/pasdjo-rounds-v2.csv", header=TRUE, 
              sep=",", na.strings="NA", dec=".", strip.white=TRUE)


(v1ScoreDistributionPlot <- ggplot(v1LongForm, aes(condition)) +
    geom_bar(aes(fill=factor(score)), position = position_stack(reverse = TRUE), color="white") + 
    coord_flip() + 
    theme(legend.position = "top", text = element_text(size=20)) + 
    labs(x="Condition",y="N") +
    scale_fill_discrete(name="Score") +
    theme(text=element_text(family="ArialMT"))
  );


savePlot(v1ScoreDistributionPlot, "score-distribution-v1.pdf", path="graphs", width=8.4, height=3.6)

(v2ScoreDistributionPlot <- ggplot(v2LongForm, aes(condition)) +
  geom_bar(aes(fill=factor(score)), position = position_stack(reverse = TRUE), color="white") + 
    coord_flip() + 
    theme(legend.position = "top", text = element_text(size=20)) + 
    labs(x="Condition",y="N") +
    scale_fill_discrete(name="Score") + 
    theme(text=element_text(family="ArialMT"))
)
savePlot(v2ScoreDistributionPlot, "score-distribution-v2.pdf", path="graphs", width=8.4, height=2.4)
