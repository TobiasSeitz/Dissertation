## complexity

library(ggplot2)
library(extrafont)
library(readr)
library(scales)

policyData <- read_csv("~/Dissertation/statistics/policies/password-policies-germany-categorized.csv")
#policyData <- read.table("policies/password-policies-germany.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

chartData <- data.frame(table(policyData$complexity))
names(chartData) <- c("Policy","Count")
rownames(chartData)<- chartData$Policy
chartData['Variable'] <- c("Complexity","Complexity","Complexity","Complexity")
tmp <- chartData[order(chartData$Count),] 
chartData <- tmp
rm(tmp)
## from stackoverflow: https://stackoverflow.com/a/9570321/1447479

Policy <- reorder(chartData$Policy,chartData$Count);

g <- ggplot(chartData,aes(x = Variable, y = Count, fill = reorder(Policy,Count))) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  # geom_text(aes(label = Count), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(x = "Distribution", col = "Policy") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.title.y = element_blank())
g
g <- g + theme(text=element_text(family="ArialMT"))
ggsave(plot=g, filename="policies-distribution.pdf", path="policies", width=11, height=2.0)
embed_fonts("policies/policies-distribution.pdf", outfile="policies/policies-distribution.pdf")
