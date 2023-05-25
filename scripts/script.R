# Andres Barboza
# 03/27/2023

library(tidyr)
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


# TODO finish function
CountRest <- function(files, reclen = 48, window = 0.0003) {
  # Gest data in "dat" for roughly every second
  dat <- GetAct(files, reclen, window)
  ## TODO turn into cm or mm per second
  # TEMPORARY - this whole section categorizes data into day and night
  day.night <- data.frame(matrix(NA, ncol = 2, nrow = length(dat[,1])))
  colnames(day.night) <- c("time", "movement")
  day.night[,2] <- dat[,1]
  day.night[1:30000,1] <- "day"
  day.night[70001:110000,1] <- "day"
  day.night[150001:160000,1] <- "day"
  day.night[30001:70000,1] <- "night"
  day.night[110001:150000,1] <- "night"
  # Measures run lengths lower than 4.9 pixels per second, in rle format
  sequences <- rle(day.night$movement < 4.9)
  # Puts the lengths of the runs in "lengths" vector
  lengths <- sequences$lengths[sequences$values]
  ##### Needed? rest <- lengths[lengths > 59] for filter
  ## TODO get threshold for inactivity to be rest
  # Puts data on rest dataframe, could be done in less lines
  rest.df <- data.frame(matrix(NA, nrow = length(lengths), ncol = 1))
  colnames(rest.df) <- c("rlength")
  rest.df$rlength <- lengths
  
  
}


yp.csv <- c("../data/YP-01-02-mar31DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/YP-03-04-apr04DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/YP-05-06-apr07DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "../data/Yp-07-08-apr11DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")

wtbs.csv <- c("../data/WT-BS-01-02-may19DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
              "../data/WT-BS-03-04-apr18DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")


yp.act <- GetAct(yp.csv, 48, 0.2)
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



