# Andres Barboza
# 03/27/2023

library(tidyr)
library(dplyr)
library(ggplot2)

GetMovement <- function(x, reclen, window, stat){
  Displacement <- function(x){
    dx <- x[1:(nrow(x)-1),1] - x[2:nrow(x),1]
    dy <- x[1:(nrow(x)-1),2] - x[2:nrow(x),2]
    d <- sqrt((dx^2)+(dy^2))
    return(d)
  }
  
  # x should be a 3 column data frame with columns containing x and y positions 
  # followed by a column with the likelihood of correct state
  #
  # reclen a numeric describing the length of recording in hours
  #
  # the window size over which we evaluate movement in hours
  #
  # the stat you want in each window (mean, sum, etc.)
  x <- x %>% fill(colnames(x), .direction = "up")
  delta <- Displacement(x)
  binsize <- floor(length(delta)/(reclen/window))
  starts <- round(seq(from=1, by=binsize, length.out=reclen/window))
  movement <-c()
  for(i in 1:length(starts)){
    movement[i] <- sum(delta[starts[i]:(starts[i]+binsize)])
  }
  return(movement)
}
GetData <- function(files, reclen, window){
  
  bins <- reclen/window
  dat <- matrix(NA, (length(files)*2), bins)
  counter <- 0
  
  for(i in 1:length(files)){
    video <- read.csv(files[i], header=T, na.strings=c(""," ","NA"))[-c(1:7),c(2:4,32:34)]
    b1 <- video[,1:3]
    b1[,1] <- as.numeric(b1[,1])
    b1[,2] <- as.numeric(b1[,2])
    b2 <- video[,4:6]
    b2[,1] <- as.numeric(b2[,1])
    b2[,2] <- as.numeric(b2[,2])
    
    counter <- counter + 1
    dat[counter, 1:bins] <- GetMovement(b1, reclen = reclen, window = window, stat="sum")
    counter <- counter + 1
    dat[counter, 1:bins] <- GetMovement(b2, reclen = reclen, window = window, stat="sum")
  }
  return(dat)
}
GetAct <- function(files, reclen, window){
  
  dat <- GetData(files, reclen, window)
  bins <- reclen/window
  upper <- lower <- c()
  for(i in 1:bins){
    x <- t.test(dat[,i])$conf.int
    lower[i] <- x[1]
    upper[i] <- x[2]
  }
  df <- data.frame(colMeans(dat), lower, upper)
  colnames(df)[1] <- "activity"
  
  return(df)
}
CountRest <- function(files, drift = 4.9, runlength = 60, reclen = 48, window = 0.0003) {
  # Gest data in "dat" for roughly every second
  dat <- GetData(files, reclen, window)
  ## TODO turn into cm or mm per second
  bouts <- data.frame(matrix(NA, ncol = 2, nrow = length(dat[,1])))
  colnames(bouts) <- c("total", "mean")
  for (i in 1:length(dat[,1])) {
    sequences <- rle(dat[i,] < drift)
    for (j in 1:length(sequences$lengths)) {
      runs <- c()
      runs <- sequences$lengths[sequences$lengths > runlength]
    }
    #lengths <- sequences$lengths
    bouts[i,1] <- sum(runs)
    bouts[i,2] <- mean(runs)
  }
  bouts[,1] <- (bouts/3600)/2
  return(bouts)
  
  
  # # TEMPORARY - this whole section categorizes data into day and night
  # day.night <- data.frame(matrix(NA, ncol = 2, nrow = length(dat[,1])))
  # colnames(day.night) <- c("time", "movement")
  # day.night[,2] <- dat[,1]
  # day.night[1:30000,1] <- "day"
  # day.night[70001:110000,1] <- "day"
  # day.night[150001:160000,1] <- "day"
  # day.night[30001:70000,1] <- "night"
  # day.night[110001:150000,1] <- "night"
  # # Measures run lengths lower than 4.9 pixels per second, in rle format
  # sequences <- rle(day.night$movement < 4.9)
  # # Puts the lengths of the runs in "lengths" vector
  # lengths <- sequences$lengths[sequences$values]
  # ##### Needed? rest <- lengths[lengths > 59] for filter
  # ## TODO get threshold for inactivity to be rest
  # # Puts data on rest dataframe, could be done in less lines
  # rest.df <- data.frame(matrix(NA, nrow = length(lengths), ncol = 1))
  # colnames(rest.df) <- c("rlength")
  # rest.df$rlength <- lengths
  
  
}

## TODO measure diurnality and "crepuscularity"
Crepuscularity <- function(files, reclen = 48, window = 0.2) {
  # Get data for strain
  dat <- GetAct(files, reclen, window)
  # create df for data labeled by time
  day.night <- data.frame(matrix(NA, ncol = 3, nrow = length(dat[,1])))
  colnames(day.night) <- c("time", "movement")
  # assign time for each second
  #day.night[,2] <- dat[,1]
  
  ##notes
  # create final df, for day-night, and including twilight
  day.night <- data.frame(matrix(NA, ncol = 3, nrow = length(dat[,1])))
  colnames(day.night) <- c("time", "movement", "strain")
  
  #get act
  yp.act <- GetAct(yp.csv, 48, 0.2)
  sr.act <- GetAct(sr.csv, 48, 0.2)
  wtbs.act <- GetAct(wtbs.csv, 48, 0.2)
  #create day and night df
  yp.dn <- data.frame(matrix(NA, ncol = 3, nrow = 240))
  colnames(yp.dn) <- c("time", "movement", "strain")
  yp.dn[,2] <- yp.act[,1]
  sr.dn <- data.frame(matrix(NA, ncol = 3, nrow = 240))
  colnames(sr.dn) <- c("time", "movement", "strain")
  sr.dn[,2] <- sr.act[,1]
  wtbs.dn <- data.frame(matrix(NA, ncol = 3, nrow = 240))
  colnames(wtbs.dn) <- c("time", "movement", "strain")
  wtbs.dn[,2] <- sr.act[,1]
  # assign time and strain in dn.df
  yp.dn[1:45,1] <- "day"
  yp.dn[46:105,1] <- "night"
  yp.dn[106:165,1] <- "day"
  yp.dn[166:225,1] <- "night"
  yp.dn[226:240,1] <- "day"
  yp.dn[,3] <- "YellowPlakat"
  sr.dn[1:45,1] <- "day"
  sr.dn[46:105,1] <- "night"
  sr.dn[106:165,1] <- "day"
  sr.dn[166:225,1] <- "night"
  sr.dn[226:240,1] <- "day"
  sr.dn[,3] <- "SuperRed"
  wtbs.dn[1:45,1] <- "day"
  wtbs.dn[46:105,1] <- "night"
  wtbs.dn[106:165,1] <- "day"
  wtbs.dn[166:225,1] <- "night"
  wtbs.dn[226:240,1] <- "day"
  wtbs.dn[,3] <- "BettaSplendensWT"
  #merge into day.night
  
  # 
  yp.dnt[46:55,1] <- "twilight"
  yp.dnt[106:115,1] <- "twilight"
  yp.dnt[166:175,1] <- "twilight"
  yp.dnt[226:235,1] <- "twilight"
  sr.dnt[46:55,1] <- "twilight"
  sr.dnt[106:115,1] <- "twilight"
  sr.dnt[166:175,1] <- "twilight"
  sr.dnt[226:235,1] <- "twilight"
  wtbs.dnt[46:55,1] <- "twilight"
  wtbs.dnt[106:115,1] <- "twilight"
  wtbs.dnt[166:175,1] <- "twilight"
  wtbs.dnt[226:235,1] <- "twilight"
  twilight <- rbind(yp.dnt, sr.dnt, wtbs.dnt)
  
  minmax <- data.frame(matrix(NA, ncol = 4, nrow = 9))
  colnames(minmax) <- c("min", "max", "time", "strain")
  
  minmax[7,2] <- max(wtbs.dnt[wtbs.dnt$time == "day", 2])
  minmax[7,1] <- min(wtbs.dnt[wtbs.dnt$time == "day", 2])
  minmax[7,3] <- "day"
  minmax[7,4] <- "BettaSplendensWT"
  
  # read in the data as twilight
  
  
  
  fit <- glm(movement ~ time + strain, data=twilight)
  summary(fit)
  step(fit)
}






yp.csv <- c("../data/YP-01-02-mar31DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/YP-03-04-apr04DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/YP-05-06-apr07DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/Yp-07-08-apr11DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")

sr.csv <- c("../data/SR-01-02-May23DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/SR-03-04-May26DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/SR-05-06-May30DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")

wtbs.csv <- c("../data/WT-BS-01-02-may19DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
              "../data/WT-BS-03-04-apr18DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
              "../data/WT")


yp.act <- GetAct(yp.csv, 48, 0.2)
sr.act <- GetAct(sr.csv, 48, 0.2)
wtbs.act <- GetAct(wtbs.csv, 48, 0.2)



## PLOTTING

# Plot locomotion through time
Act.Plot <- function(df, reclen, window) {
  ggtheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                panel.border=element_blank(),
                                axis.line = element_line(colour="grey30"),
                                axis.title = element_text(colour="grey20"),
                                axis.text = (element_text(colour="grey30")),
                                legend.title = element_text(colour="grey20"),
                                legend.text = element_text(colour="grey30"))
  
  bins <- reclen/window
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

    