#install.packages("visreg")
#install.packages("texreg")

library(mgcv)
library(ggplot2)
library(visreg)
library(plyr)
#package um latex tabellen auszugeben
library("texreg")

# BibTex citations
# print(citation(package = "mgcv"), bibtex = TRUE)

# Importiere Daten
data.difficulty <- read.csv("data/data_creationDifficult_emoji12 (121 Datasets).CSV",
                  sep = ";", dec = ",")
data.ratings <- read.csv("data/data_creationRating_emoji12 (103 Datasets).CSV",
                     sep = ";", dec = ",")

# make colnames English
names(data.difficulty)[names(data.difficulty) == "Geschlecht"] <- "Gender"
names(data.difficulty)[names(data.difficulty) == "Alter"] <- "Age"
names(data.difficulty)[names(data.difficulty) == "Openess"] <- "Openness"
names(data.ratings)[names(data.ratings) == "Geschlecht"] <- "Gender"
names(data.ratings)[names(data.ratings) == "Alter"] <- "Age"
names(data.ratings)[names(data.ratings) == "Openess"] <- "Openness"

# give good labels to abbreviated column names
# TODO.
# positionLabels <- c("Task Number (emoji12)", "Task Number (2word12)", "Task Number (3class12)")
# labels(data.difficulty,which=c("emojiPos","twoWordPos","threeClassPos")) <- positionLabels;

# Variablen richtig kodieren
data.difficulty$Gender[data.difficulty$Gender == 3] <- NA
data.difficulty$Gender <- factor(data.difficulty$Gender,levels=c(1,2),labels=c("Male","Female"))
data.difficulty$IT <- factor(data.difficulty$IT,levels=c(1,2),labels=c("Yes","No"))
data.difficulty$emojiPos <- factor(data.difficulty$emojiPos)
data.difficulty$twoWordPos <- factor(data.difficulty$twoWordPos)
data.difficulty$threeClassPos <- factor(data.difficulty$threeClassPos)

## calculate average difficulty
data.difficulty$meanDiff <- rowMeans(data.difficulty[which(colnames(data.difficulty) == "emojiDif" | colnames(data.difficulty) == "twoWordDif" | colnames(data.difficulty) == "threeClassDif")], na.rm=TRUE)

data.ratings$Gender[data.ratings$Geschlecht == 3] <- NA
data.ratings$Gender <- factor(data.ratings$Gender,levels=c(1,2),labels=c("Male","Female"))
data.ratings$IT <- factor(data.ratings$IT,levels=c(1,2),labels=c("Yes","No"))
data.ratings$emojiPos <- factor(data.ratings$emojiPos)
data.ratings$twoWordPos <- factor(data.ratings$twoWordPos)
data.ratings$threeClassPos <- factor(data.ratings$threeClassPos)


meanDifficulty <- gam(meanDiff ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                        s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
                      data = data.difficulty)
summary(meanDifficulty)
plot(meanDifficulty, pages = 1, jit=TRUE, scale = 0)
# Model for creationDifficulty -- emoiji12 ----------------------------------------------------------
# Assume non-linear effects for all continous variables (all but gender, IT, and emojiPos)
emojiDifficulty <- gam(emojiDif ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
              s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
            data = data.difficulty)
summary(emojiDifficulty)
texreg(emojiDifficulty)
#plot(emojiDifficulty, pages = 1, jit=TRUE, scale = 0)

# use plot = FALSE to get plot data from visreg without plotting
emojiDifficultyPlotData <- visreg(emojiDifficulty, type = "contrast", plot = FALSE)

# The output from visreg is a list of the same length as the number of 'x' variables,
#   so we use ldply to pick the objects we want from the each list part and make a dataframe: 
smoothsEmojiDifficulty <- ldply(emojiDifficultyPlotData, function(part)   
  data.frame(Variable = part$meta$x, 
             x=part$fit[[part$meta$x]], 
             smooth=part$fit$visregFit, 
             lower=part$fit$visregLwr, 
             upper=part$fit$visregUpr))

#### subset datafor control variables
smoothsEmojiDifficulty.control <- smoothsEmojiDifficulty[
  which(smoothsEmojiDifficulty$Variable == 'Age' |
    smoothsEmojiDifficulty$Variable == 'Gender' |
    smoothsEmojiDifficulty$Variable == 'IT' |
    smoothsEmojiDifficulty$Variable == 'emojiPos'),];
smoothsEmojiDifficulty.bigfive <- smoothsEmojiDifficulty[
  which(smoothsEmojiDifficulty$Variable == 'Extraversion' |
          smoothsEmojiDifficulty$Variable == 'Agreeableness' |
          smoothsEmojiDifficulty$Variable == 'Conscientiousness' |
          smoothsEmojiDifficulty$Variable == 'Neuroticism' |
          smoothsEmojiDifficulty$Variable == 'Openness'
        ),];


## make sure the facets will have a good title afterwards.
levels(smoothsEmojiDifficulty.control$Variable)[levels(smoothsEmojiDifficulty.control$Variable) == 'emojiPos'] <- "Task Number (emoji12)" 

# The ggplot:
int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 

width <- 10
height <- 3.0

(pDCEmojiB5 <- ggplot(smoothsEmojiDifficulty.bigfive, aes(x, smooth)) + geom_line() +
  geom_line(aes(y=lower), linetype="dashed") + 
  geom_line(aes(y=upper), linetype="dashed") + 
  scale_x_continuous(breaks=int_breaks)+
  facet_grid(. ~ Variable, scales = "free_x") +
  labs(y="Difficulty to create emoji password", x="Trait Score"))
ggsave(plot=pDCEmojiB5, filename="dc-emoji-b5.pdf", path="graphs", width=width, height=height)
(pDCEmojiCTRL <- ggplot(smoothsEmojiDifficulty.control, aes(x, smooth)) + geom_line() +
    geom_line(aes(y=lower), linetype="dashed") + 
    geom_line(aes(y=upper), linetype="dashed") + 
    scale_x_continuous(breaks=int_breaks)+
    facet_grid(. ~ Variable, scales = "free_x") +
    labs(y="Difficulty to create emoji password", x="Control Variable Level")
  )
ggsave(plot=pDCEmojiCTRL, filename="dc-emoji-control.pdf", path="graphs", width=width, height=height)

####### 
# if the summary of the "approximate significance of smooth terms" has an 
# estimated degrees of freedom (edf) greater than 1, it means that there is a non-linear effect / curve.
# in that case it could make sense to look at the curves in higher detail. 
#emoji
model1_emoji <- gam(emojiDif ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
                Conscientiousness + Neuroticism + s(Openness) + emojiPos,
              data = data.difficulty)
plot(model1_emoji, pages = 1, jit=TRUE)
summary(model1_emoji)

#twoWord
model1_twoWord <- gam(twoWordDif ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                        s(Conscientiousness) + s(Neuroticism) + s(Openness) + twoWordPos,
                      data = data.difficulty)
summary(model1_twoWord)
plot(model1_twoWord, pages = 1, jit=TRUE, scale = 0)
#threeClass
model1_threeClass <- gam(threeClassDif ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
                        Conscientiousness + Neuroticism + s(Openness) + threeClassPos,
                      data = data.difficulty)
summary(model1_threeClass)
plot(model1_threeClass, pages = 1, jit=TRUE, scale =0)

# Code fuer Interaktionen:
# lineare Effekte: Neuroticism*Gender
# nichtlineare Effekte: s(Neuroticism, by = Gender)
model2 <- gam(emojiDif ~ Age + Gender + IT + Extraversion + s(Agreeableness) +
               Conscientiousness + Neuroticism + s(Openness) + emojiPos,
             data = data.difficulty)
plot(model2, pages = 1, jit=TRUE)
summary(model2)


texreg(list(model1_emoji, model1_twoWord, model1_threeClass))


# Modell fuer creationRating --------------------------------------------------------------
# Logistische Regression 

data.ratings$emojiRat[data.ratings$emojiRat == 0] <- NA
data.ratings$emojiRat <- factor(data.ratings$emojiRat)
data.ratings$emojiRat_binary <- factor(as.numeric(data.ratings$emojiRat == 1))

data.ratings$twoWordRat[data.ratings$twoWordRat == 0] <- NA
data.ratings$twoWordRat <- factor(data.ratings$twoWordRat)
data.ratings$twoWordRat_binary <- factor(as.numeric(data.ratings$twoWordRat == 1))

data.ratings$threeClassRat[data.ratings$threeClassRat == 0] <- NA
data.ratings$threeClassRat <- factor(data.ratings$threeClassRat)
data.ratings$threeClassRat_binary <- factor(as.numeric(data.ratings$threeClassRat == 1))

model3 <- gam(emojiRat_binary ~ s(Alter) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
              family = binomial(link = "logit"), data = data.ratings)
plot(model3, pages = 1, jit = TRUE)
plot(model3, pages = 1, jit = TRUE, scale = 0)
summary(model3)

# k=5 beschreibt, dass Maximal ein Polynom vom Grad 5 verwendet werden soll.
model2_emojiRat <- gam(emojiRat_binary ~ s(Alter, k=5) + Gender + IT + Extraversion + Agreeableness +
                Conscientiousness + s(Neuroticism, k=5) + Openness + emojiPos,
              family = binomial(link = "logit"), data = data.ratings)
plot(model2_emojiRat, pages = 1, scale = 0, jit = TRUE)
summary(model2_emojiRat)

model2_twoWordRat <- gam(twoWordRat_binary ~ s(Alter, k=5) + Gender + IT + Extraversion + s(Agreeableness, k=5) +
                         Conscientiousness + Neuroticism + Openness + twoWordPos,
                       family = binomial(link = "logit"), data = data.ratings)
plot(model2_twoWordRat, pages = 1, scale = 0, jit = TRUE)
summary(model2_twoWordRat)

model2_threeClassRat <- gam(threeClassRat_binary ~ Alter + Gender + IT + Extraversion + Agreeableness +
                         Conscientiousness + s(Neuroticism, k=5) + Openness + threeClassPos,
                       family = binomial(link = "logit"), data = data.ratings)
plot(model2_threeClassRat, pages = 1, scale = 0, jit = TRUE)
summary(model2_threeClassRat)

texreg(list(model2_emojiRat, model2_twoWordRat, model2_threeClassRat))

length (data.ratings$emojiRat_binary)
exp(1)

#die chance emoji auf platz 1 zu wählen steigt bei hohen agree werten. 
#so ist sie z.b. bei einem unterschied von 1 exp(1) mal so groß

# Repeated measures ANOVA (Untersuchung durch lineares gemischtes Modell) ---
dat1 <- data.difficulty[,c("emojiDif","twoWordDif","threeClassDif")]
dat2 <- data.difficulty[,c("emojiPos","twoWordPos","threeClassPos")]
dat1$Person <- dat2$id <- 1:nrow(dat1)
# Datensatz in long-Format bringen
dat1_long <- reshape(dat1, direction = "long", varying = 1:3,
                    v.names = "Dif", timevar = "PW_Art", idvar = "Person")
dat2_long <- reshape(dat2, direction = "long", varying = 1:3,
                     v.names = "Pos", timevar = "PW_Art", idvar = "Person")
dat_long <- merge(dat1_long, dat2_long, by = c("Person","PW_Art"))
head(dat_long)

dat_long$PW_Art[dat_long$PW_Art == 1] <- "emoji"
dat_long$PW_Art[dat_long$PW_Art == 2] <- "twoWord"
dat_long$PW_Art[dat_long$PW_Art == 3] <- "threeClass"
dat_long$PW_Art <- factor(dat_long$PW_Art)


# Ueberpruefen, ob Zuordnung noch stimmt
all.equal(sort(dat_long$Dif[dat_long$PW_Art == "emoji"]), sort(dat1$emojiDif))
all.equal(sort(dat_long$Dif[dat_long$PW_Art == "twoWord"]), sort(dat1$twoWordDif))
all.equal(sort(dat_long$Dif[dat_long$PW_Art == "threeClass"]), sort(dat1$threeClassDif))

str(dat_long)
library(lme4)

# emoji ist die abhängige Variable
#m5 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long)
dat_long$PW_Art <- relevel(dat_long$PW_Art, "emoji")
m5 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long,
           contrasts = list(Pos = contr.sum))
summary(m5)

# mache twoWord zur abhängigen Variable
dat_long$PW_Art <- relevel(dat_long$PW_Art, "twoWord")
m6 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long)
summary(m6)

# mache threeClass zur abhängigen Variable
dat_long$PW_Art <- relevel(dat_long$PW_Art, "threeClass")
m7 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long)
summary(m7)


texreg(m5)

exp(0.86)

#pie Chart
slices <- c(11, 17, 32, 40)
lbls <- c("[1]", "[2]", "[3]", "[4]")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

