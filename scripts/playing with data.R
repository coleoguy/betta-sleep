
library(ggplot2)
library(dplyr)
dat <- rbind(read.csv("results/yp.csv"), read.csv("results/sr.csv"), read.csv("results/wtbs.csv"))
dat$strains <- as.factor(c(rep("Yellow Plakat", 8), rep("Super Red", 6), rep("Wild-type", 8)))
dat <- data.frame(c(dat$day,dat$twilight,dat$night),
                  c(factor(rep(c("Day","Twilight","Night"), each=22))),
                  c(rep(dat$strains, times=3)))
colnames(dat) <- c("Locomotion", "Time", "Strain")
dat$Time <- factor(dat$Time, levels = c("Day","Twilight","Night"))
dat$Locomotion <- dat$Locomotion/12


ggplot(dat, aes(x=Strain,y=Locomotion, fill=Time)) +
  geom_dotplot(binaxis = "y",stackdir = "center",position="dodge",dotsize=0.7) +
  theme_bw() +
  scale_fill_manual(values = c("#B9DBF4", "#aaAAD3","#155289"))+
  stat_summary(fun = mean, geom = "point", shape = 95, size = 10, color = "#ff0000", position = position_dodge(width = 0.9))+
  guides(fill = guide_legend(override.aes = list(shape = NA)))

