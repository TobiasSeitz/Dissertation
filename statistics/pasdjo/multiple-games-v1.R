library(lmtest);
library(zoo);
library(ggplot2);
library(extrafont);
gamesMultiplePlayers <- read.table("data/pasdjo-multiple-players-wide.csv", header=TRUE, 
                                  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

v1GamesMultiplePlayers <- read.table("data/pasdjo-v1-multiple-players-wide.csv", header=TRUE, 
                             sep=",", na.strings="NA", dec=".", strip.white=TRUE)

v2GamesMultiplePlayers <- read.table("data/pasdjo-v2-multiple-players-wide.csv", header=TRUE, 
                                     sep=",", na.strings="NA", dec=".", strip.white=TRUE)


# assumption checks:
# auto correlation:
dwtest(percent ~ gameIndex, alternative="two.sided", data=gamesMultiplePlayers)
dwtest(percent ~ gameIndex, alternative="two.sided", data=v1GamesMultiplePlayers)
dwtest(percent ~ gameIndex, alternative="two.sided", data=v2GamesMultiplePlayers)

# try to fit a linear model 
linearModel <- lm(percent~gameIndex, data=gamesMultiplePlayers)
linearModelv1 <- lm(percent~gameIndex, data=v1GamesMultiplePlayers)
linearModelv2 <- lm(percent~gameIndex, data=v2GamesMultiplePlayers)
summary(linearModel)
summary(linearModelv1)
summary(linearModelv2)

# local regression (smoothing the trend)
# loessMod <- loess(percent ~ gameIndex, data=pasdjoGamesMultiplePlayers, span=0.50) 

# outlierTest(percentByGameIndex)



### graphics overall
p <- ggplot(gamesMultiplePlayers, aes(gameIndex,percent));
(p <- p + 
  scale_y_continuous("Achieved Percent",limits=c(40,100),breaks=seq(0,100,10)) +
  geom_smooth(method = "loess", se = TRUE) +
  geom_point(aes(colour= factor(userID), size = total / 10), alpha = 1/2) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.y = element_text(colour = "grey60", size = 13)) +
  theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
   theme(axis.title.x = element_text(color="#434343")) +
   theme(axis.title.y = element_text(color="#434343")) +
  labs(x = "Number of Games Played") +
  theme(text=element_text(family="Roboto"))
)

ggsave(plot=p, filename="player-progress-multiplayers.pdf", path="graphs", width = 10, height = 4)
embed_fonts("graphs/player-progress-multiplayers.pdf", outfile="graphs/player-progress-multiplayers.pdf")



### graphics (random player)
randomPlayer <- gamesMultiplePlayers[61:70,];
p <- ggplot(randomPlayer, aes(gameIndex,percent));
(p <- p + 
   scale_y_continuous("Achieved Percent",limits=c(40,100),breaks=seq(0,100,10)) +
   scale_x_continuous("Number of Games Played",limits=c(1,16),breaks=seq(0,16,4)) +
   #geom_smooth(method = "loess", se = TRUE) +
   geom_point(aes(colour= factor(userID), size = total / 10), alpha = 1/2) +
   theme(legend.position = "none") +
   theme(text = element_text(size = 20)) +
   theme(axis.text.y = element_text(colour = "grey60", size = 13)) +
   theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
   theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
   theme(axis.title.x = element_text(color="#434343")) +
   theme(axis.title.y = element_text(color="#434343")) +
   theme(text=element_text(family="Roboto"))
)
ggsave(plot=p, filename="player-progress-multiplayers-00.pdf", path="graphs", width = 10, height = 4)
embed_fonts("graphs/player-progress-multiplayers-00.pdf", outfile="graphs/player-progress-multiplayers-00.pdf")



### v1 graphics
p <- ggplot(v1GamesMultiplePlayers, aes(gameIndex,percent));
p <- p + 
  scale_y_continuous("Achieved Percent",limits=c(40,100),breaks=seq(0,100,10)) +
  geom_smooth(method = "loess", se = TRUE) +
  geom_point(aes(colour= factor(userID), size = total / 10), alpha = 1/2) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.y = element_text(colour = "grey60", size = 13)) +
  theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  labs(x = "Number of Games Played")

p <- p + theme(text=element_text(family="ArialMT"))
p
ggsave(plot=p, filename="v1-player-progress-multiplayers.pdf", path="../graphs", width = 8.4, height = 4)
embed_fonts("../graphs/v1-player-progress-multiplayers.pdf", outfile="../graphs/v1-player-progress-multiplayers.pdf")


### v2 graphics
p <- ggplot(v2GamesMultiplePlayers, aes(gameIndex,percent));
p <- p + 
  scale_y_continuous("Achieved Percent",limits=c(40,100),breaks=seq(0,100,10)) +
  geom_smooth(method = "loess", se = TRUE) +
  # geom_smooth(method = "loess", se = TRUE) +
  geom_point(aes(colour= factor(userID), size = total / 10), alpha = 1/2) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.y = element_text(colour = "grey60", size = 13)) +
  theme(axis.text.x = element_text(colour = "grey60", size = 13)) +
  theme(axis.ticks.x = element_line(colour = "grey87"), axis.ticks.length = unit(0.2, "cm")) +
  labs(x = "Number of Games Played")

p <- p + theme(text=element_text(family="ArialMT"))
p
ggsave(plot=p, filename="v2-player-progress-multiplayers.pdf", path="graphs", width = 9.59, height = 4)
embed_fonts("graphs/v2-player-progress-multiplayers.pdf", outfile="graphs/v2-player-progress-multiplayers-00.pdf")
