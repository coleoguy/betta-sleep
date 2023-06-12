# Andres Barboza
# 06/06/2023

library(ggplot2)

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

## Plot activity with rest bouts (R)
# Yellow Plakat
plot((yp.dat$activity/12)~yp.dat$time, type="l", col = "#ffb301", ylim=c(-5,90), xaxt = "n", xlab = "ZT", ylab = "Locomotion (cm min-1)")
stdsz <- max(yp.dat$bouts)/1.5
for(i in 1:nrow(yp.dat)){
  points(x=yp.dat$time[i], y=0, cex=yp.dat$bouts[i]/stdsz, pch="|")
}
rect(xleft = 9, ybottom = -5, xright = 21, ytop = 90, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
rect(xleft = 33, ybottom = -5, xright = 45, ytop = 90, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
##axis(side = 1, at = c(1:240), labels = rep(1:120, each=2)+2)
# Super Red
plot((sr.dat$activity/12)~sr.dat$time, type="l", col = "#cc0000", ylim=c(-5,90), xlab = "ZT", ylab = "Locomotion (cm min-1)")
stdsz <- max(sr.dat$bouts)/1.5
for(i in 1:nrow(sr.dat)){
  points(x=sr.dat$time[i], y=0, cex=sr.dat$bouts[i]/stdsz, pch="|")
}
rect(xleft = 9, ybottom = 5, xright = 21, ytop = 90, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
rect(xleft = 33, ybottom = 5, xright = 45, ytop = 90, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
# Wild-Type (Betta Splendens)
plot((wtbs.dat$activity/12)~wtbs.dat$time, type="l", col = "#77aa22", ylim=c(-5,90), xlab = "ZT", ylab = "Locomotion (cm min-1)")
stdsz <- max(wtbs.dat$bouts)/1.5
for(i in 1:nrow(wtbs.dat)){
  points(x=wtbs.dat$time[i], y=0, cex=wtbs.dat$bouts[i]/stdsz, pch="|")
}
rect(xleft = 9, ybottom = 5, xright = 21, ytop = 90, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
rect(xleft = 33, ybottom = 5, xright = 45, ytop = 90, col = adjustcolor("gray", alpha.f = 0.3), border = NA)
}


###### In development #####


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


