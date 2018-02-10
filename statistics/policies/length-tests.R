### length boxplots


library(readxl)
library(ggplot2)
library(extrafont)
length_categories <- read_excel("~/Dissertation/statistics/policies/lengths.xlsx")

colnames(length_categories) <- c("Bound","n");

# plots

distr_length_max <- subset(length_categories, Bound=="Max")
distr_length_min <- subset(length_categories, Bound=="Min")


p_min <- ggplot(distr_length_min, aes(factor(Bound),n))
p_min <- p_min + 
  geom_violin()+
  coord_flip()+
  scale_y_continuous(name="Minimum Number of Characters", breaks=c(1,2,3,4,5,6,7,8,9,10,11))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())

p_max <- ggplot(distr_length_max, aes(factor(Bound),n))
p_max <- p_max + 
  geom_violin()+
  coord_flip()+
  scale_y_continuous(name="Maximum Number of Characters", breaks=c(0,10,20,30,40,50,60,80,100,150))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())

p_max <- p_max + theme(text=element_text(family="ArialMT"))
p_min <- p_min + theme(text=element_text(family="ArialMT"))

ggsave(plot=p_max, filename="length-max-violin.pdf", path="policies", width=11, height=2.0)
embed_fonts("policies/length-max-violin.pdf", outfile="policies/length-max-violin.pdf")

ggsave(plot=p_min, filename="length-min-violin.pdf", path="policies", width=11, height=2.0)
embed_fonts("policies/length-min-violin.pdf", outfile="policies/length-min-violin.pdf")

(p <- ggplot(length_categories, aes(Bound,n)) + 
    geom_violin()+
    coord_flip()+
    scale_y_continuous(name="Maximum Number of Characters", breaks=c(0,10,20,30,40,50,60,80,100,150))+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank())
)
