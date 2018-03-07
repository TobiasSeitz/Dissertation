library(ggplot2);
library(extrafont);

### creating a timeline histogram


games <- read.table("../data/games.csv", header=TRUE, 
                                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)


p <- ggplot(games, aes(isodate));
p <- p + geom_bar(aes(fill= user)) + 
  theme(legend.position = "none", axis.text.x=element_blank()) +
  theme(axis.title.y = element_text()) + 
  theme(axis.title.x = element_text()) +
  labs(x = "Date (Dec 05 2016 - Jan 23 2018)", y = 'Number of games played')
;
p <- p + theme(text=element_text(family="ArialMT"))
p

ggsave(plot=p, filename="games-per-day.pdf", path="../graphs", width = 8.4, height = 4);
embed_fonts("../graphs/games-per-day.pdf", outfile="../graphs/games-per-day.pdf");
