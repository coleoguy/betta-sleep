
library(ggplot2)
dat <- rbind(read.csv("yp.csv"), read.csv("sr.csv"), read.csv("wtbs.csv"))[,-1]
dat$strains <- as.factor(c(rep("yellow plakat", 8), rep("super red", 6), rep("wild-type", 8)))
dat <- data.frame(c(dat$day,dat$twilight,dat$night),
                  c(factor(rep(c("day","twilight","night"), each=22))),
                  c(rep(dat$strains, times=3)))
colnames(dat) <- c("activity", "time", "strain")
dat$time <- factor(dat$time, levels = c("day","twilight","night"))
ggplot(dat, aes(x=strain,y=activity, color=time)) +
  geom_dotplot(binaxis = "y",stackdir = "center",position="dodge") +
  theme_bw() +
  scale_fill_manual(breaks = c("day", "twilight", "night"), 
                    values=c("red", "blue", "green"))

