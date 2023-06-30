# Andres Barboza
# 06/06/2023

library(ggplot2)
library(dplyr)
library(Ternary)

# Reading in data

# Theme
ggtheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              panel.border=element_blank(),
                              axis.line = element_line(colour="grey30"),
                              axis.title = element_text(colour="grey20"),
                              axis.text = (element_text(colour="grey30")),
                              legend.title = element_text(colour="grey20"),
                              legend.text = element_text(colour="grey30"))


## Plot activity with rest bouts (R)
####### TODO change from environment to read-in results
#ZT axis
ZT <- rep(seq(0, 23), times = 10)
ZT <- ZT[-1]
ZT <- ZT[-1]
ZT <- ZT[-1]
ZT <- c(ZT,0,1,2)
# Yellow Plakat
plot((yp.rest$activity/12)~yp.rest$time, type="l", col = "#ffb301", lwd = 2, ylim=c(-5,90), xaxt = "n", xlab = "ZT", ylab = "Locomotion (cm min-1)")
stdsz <- max(yp.rest$bouts)/2
for(i in 1:nrow(yp.rest)){
  points(x=yp.rest$time[i], y=0, cex=yp.rest$bouts[i]/stdsz, pch="|")
}
rect(xleft = 9, ybottom = -10, xright = 21, ytop = 95, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
rect(xleft = 33, ybottom = -10, xright = 45, ytop = 95, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
axis(side = 1, at = c(0:239), labels = ZT)
# Super Red
plot((sr.rest$activity/12)~sr.rest$time, type="l", col = "#cc0000", lwd = 2, ylim=c(-5,90), xaxt = "n", xlab = "ZT", ylab = "Locomotion (cm min-1)")
stdsz <- max(sr.rest$bouts)/2
for(i in 1:nrow(sr.rest)){
  points(x=sr.rest$time[i], y=0, cex=sr.rest$bouts[i]/stdsz, pch="|")
}
rect(xleft = 9, ybottom = -10, xright = 21, ytop = 95, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
rect(xleft = 33, ybottom = -10, xright = 45, ytop = 95, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
axis(side = 1, at = c(0:239), labels = ZT)
# Wild-Type (Betta Splendens)
plot((wtbs.rest$activity/12)~wtbs.rest$time, type="l", col = "#77aa22", lwd = 2, ylim=c(-5,90), xaxt = "n", xlab = "ZT", ylab = "Locomotion (cm min-1)")
stdsz <- max(wtbs.rest$bouts)/2
for(i in 1:nrow(wtbs.rest)){
  points(x=wtbs.rest$time[i], y=0, cex=wtbs.rest$bouts[i]/stdsz, pch="|")
}
rect(xleft = 9, ybottom = -10, xright = 21, ytop = 95, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
rect(xleft = 33, ybottom = -10, xright = 45, ytop = 95, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
axis(side = 1, at = c(0:239), labels = ZT)

## Dot Plot
#reading in results, binding strains
dat <- rbind(read.csv("results/yp.csv"), read.csv("results/sr.csv"), read.csv("results/wtbs.csv"))
dat$strains <- as.factor(c(rep("Yellow Plakat", 8), rep("Super Red", 6), rep("Wild-type", 8)))
dat <- data.frame(c(dat$day,dat$twilight,dat$night),
                  c(factor(rep(c("Day","Twilight","Night"), each=22))),
                  c(rep(dat$strains, times=3)))
colnames(dat) <- c("Locomotion", "Time", "Strain")
dat$Time <- factor(dat$Time, levels = c("Day","Twilight","Night"))
dat$Locomotion <- dat$Locomotion/12
#plotting
ggplot(dat, aes(x=Strain,y=Locomotion, fill=Time)) +
  geom_dotplot(binaxis = "y",stackdir = "center",position="dodge",dotsize=0.7) +
  theme_bw() +
  scale_fill_manual(values = c("#B9DBF4", "#aaAAD3","#155289"))+
  stat_summary(fun = mean, geom = "point", shape = 95, size = 10, color = "#ff0000", position = position_dodge(width = 0.9))+
  guides(fill = guide_legend(override.aes = list(shape = NA)))


##Ternary Plot
df <- rbind(yp.score, sr.score, wtbs.score)
cols <- c(rgb(0.8,0.1,0.1), rgb(0.1,0.8,0.1), rgb(0.8,0.8,0.1))[as.factor(df$strain)]
TernaryPlot(atip = "Diurnality", btip = "Crepuscularity", ctip = "Nocturnality",
            alab = "Diurnality", blab = "Crepuscularity", clab = "Nocturnality")
TernaryPoints(df[1:3], col = cols)
















###### In development #####

# Plot activity with confidence intervals
## TODO Turn from function to script
Act.Plot <- function(df, reclen, window) {
  bins <- reclen/window
  # x shoud be = rep(1:240, 3)
  ggplot(df, aes(x = 1:bins, y = activity)) + 
    ggtheme +
    geom_line(col="black") + 
    geom_point() + 
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha=0.1, 
                linetype="dashed",
                color="grey") +
    annotate("rect", xmin = 9/window, xmax = 21/window, ymin = -1, ymax = max(df),
             alpha = .2,fill = "gray5") +
    annotate("rect", xmin = 33/window, xmax = 45/window, ymin = -1, ymax = max(df),
             alpha = .2,fill = "gray5")
}


# Plot day.night distributions
ggplot(day.night, aes(x=movement, color=time)) +
  ggtheme +
  geom_histogram(fill='white', alpha=0.5, position = 'identity') +
  xlim(0,10)

# Plot run-lengths 
plot(lengths, ylim = c(0,100))
abline(h = 60, col = "purple")
abline(h = 30, col = "blue")
abline(h = 20, col = "red")

# Plotting rest bouts
##Bout.plot
combined_df <- data.frame(cbind(yp.rest$mean, sr.rest$mean, wtbs.rest$mean))
colnames(combined_df) <- c("yellow plakat", "superRed", "WT")
group_colors <- c("superRed" = "red", "WT" = "green", "yellow plakat" = "yellow")
df_long <- tidyr::gather(combined_df, key = "Group", value = "Values")
summary_df <- df_long %>%
  group_by(Group) %>%
  summarize(Mean = mean(Values),
            SE = sd(Values) / sqrt(length(Values)))

ggplot(df_long, aes(x = Group, y = Values, fill = Group)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = group_colors, fill = group_colors) +
  geom_jitter(position = position_dodge(width = 0.9)) +
  #geom_errorbar(data = summary_df, aes(ymin = (Mean - SE), ymax = (Mean + SE)),
  #              width = 0.2, position = position_dodge(width = 0.9), color = "black") +
  labs(x = "Strain", y = "mean length in seconds") +
  ggtitle("mean length of rest bouts")

ggplot(df_long, aes(x = Group, y = Values, fill = Group)) +
  geom_violin() +
  scale_fill_manual(values = group_colors) +
  labs(x = "strain", y = "Hours of rest") +
  ggtitle("Hours of rest per day")


