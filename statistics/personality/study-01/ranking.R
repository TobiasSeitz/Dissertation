# ranking: depends on data from analysis.R
#############################################################################
#
# Model for calculating the likelihood of ranking a policy on a specific place
# --------------------------------------------------------------
# we use the data.ratings dataset because there was an inconsistency in the phrasing of the question for 18 people after we
# had changed to policy from 1emoji16 to 1emoji12 

# clean data -- convert 0 to NA
data.ratings$emojiRat[data.ratings$emojiRat == 0] <- NA
data.ratings$twoWordRat[data.ratings$twoWordRat == 0] <- NA
data.ratings$threeClassRat[data.ratings$threeClassRat == 0] <- NA

# create distinct levels of ratings
data.ratings$emojiRat <- factor(data.ratings$emojiRat)
data.ratings$twoWordRat <- factor(data.ratings$twoWordRat)
data.ratings$threeClassRat <- factor(data.ratings$threeClassRat)

# we model the preference for one policy or another as binary decision in each case (preferred = yes / no).
data.ratings$emojiRat_binary <- factor(as.numeric(data.ratings$emojiRat == 1))
data.ratings$twoWordRat_binary <- factor(as.numeric(data.ratings$twoWordRat == 1))
data.ratings$threeClassRat_binary <- factor(as.numeric(data.ratings$threeClassRat == 1))

# since the decision is binary, we use a logit model.
model3 <- gam(emojiRat_binary ~ s(Age) + Gender + IT + s(Extraversion) + s(Agreeableness) +
                s(Conscientiousness) + s(Neuroticism) + s(Openness) + emojiPos,
              family = binomial(link = "logit"), data = data.ratings)
#plot(model3, pages = 1, jit = TRUE)
plot(model3, pages = 1, jit = TRUE, scale = 0)
summary(model3)

# k=5 beschreibt, dass Maximal ein Polynom vom Grad 5 verwendet werden soll.
emojiRanking <- gam(emojiRat_binary ~ s(Age, k=5) + Gender + IT + Extraversion + Agreeableness +
                      Conscientiousness + s(Neuroticism, k=5) + Openness + emojiPos,
                    family = binomial(link = "logit"), data = data.ratings)
plot(emojiRanking, pages = 1, scale = 0, jit = TRUE)
summary(emojiRanking)
outputSummary(emojiRanking, "ranking-","summaries")

twoWordRanking <- gam(twoWordRat_binary ~ s(Age, k=5) + Gender + IT + Extraversion + s(Agreeableness, k=5) +
                        Conscientiousness + Neuroticism + Openness + twoWordPos,
                      family = binomial(link = "logit"), data = data.ratings)
plot(twoWordRanking, pages = 1, scale = 0, jit = TRUE)
summary(twoWordRanking)

threeClassRanking <- gam(threeClassRat_binary ~ Age + Gender + IT + Extraversion + Agreeableness +
                           Conscientiousness + s(Neuroticism, k=5) + Openness + threeClassPos,
                         family = binomial(link = "logit"), data = data.ratings)
plot(threeClassRanking, pages = 1, scale = 0, jit = TRUE)
summary(threeClassRanking)

length(data.ratings$emojiRat_binary)
exp(1)
#die chance emoji auf platz 1 zu wählen steigt bei hohen agree werten. 
#so ist sie z.b. bei einem unterschied von 1 exp(1) mal so groß


# create latex table from the three models.
texreg(list(emojiRanking, twoWordRanking, threeClassRanking),custom.model.names = c("emoji12","2word12","3class12"))
pModelComparisonplotreg <- plotreg(
  list(emojiRanking, twoWordRanking, threeClassRanking),
  custom.model.names = c("emoji12","2word12","3class12"),
  file="rankingModelComparison.pdf"
)

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


# emoji ist die abhängige Variable
#m5 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long)
dat_long$PW_Art <- relevel(dat_long$PW_Art, "emoji")
m5 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long,
           contrasts = list(Pos = contr.sum))
summary(m5)
anova(m5) # to get F values

# mache twoWord zur abhängigen Variable
dat_long$PW_Art <- relevel(dat_long$PW_Art, "twoWord")
m6 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long)
summary(m6)
anova(m6)

# mache threeClass zur abhängigen Variable
dat_long$PW_Art <- relevel(dat_long$PW_Art, "threeClass")
m7 <- lmer(Dif ~ PW_Art + Pos + (1|Person), data = dat_long)
summary(m7)
anova(m7)

texreg(m5)
exp(0.86) #?