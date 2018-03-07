## score distributions

v2LongForm <- read.table("../data/pasdjo-rounds-v2.csv", header=TRUE, 
              sep=",", na.strings="NA", dec=".", strip.white=TRUE)


p <- ggplot(v2LongForm, aes(condition));
p <- p + geom_bar(aes(fill=factor(score)), position = position_stack(reverse = TRUE)) + 
  coord_flip() + 
  theme(legend.position = "top", text = element_text(size=20)) + 
  labs(x="Condition",y="N") +
  scale_fill_discrete(name="Score")

p <- p + theme(text=element_text(family="ArialMT"))
ggsave(plot=p, filename="score-distribution-v2.pdf", path="../graphs", width=8.4, height=2.4)
embed_fonts("../graphs/score-distribution-v2.pdf", outfile="../graphs/score-distribution-v2.pdf")
p
