# Andres Barboza
# 03/27/2023

library(tidyr)

# Functions
GetMovement <- function(x, reclen, window, stat){
  Displacement <- function(x){
    dx <- x[1:(nrow(x)-1),1] - x[2:nrow(x),1]
    dy <- x[1:(nrow(x)-1),2] - x[2:nrow(x),2]
    d <- sqrt((dx^2)+(dy^2))
    # Tanks are 30.5cm each (1 ft, 61cm total width), videos are 640 pixels wide
    d <- d*(61/640)
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
GetData <- function(files, reclen = 48, window = 0.2){
  
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
GetAct <- function(dat, reclen = 48, window = 0.2){

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
CountRest <- function(files, drift = 4.5, reclen = 48, window = 0.0003) {
  
  dat <- GetData(files, reclen, window)
  drift <- drift*(61/640)
  bouts <- c()
  for (i in 1:length(dat[,1])) {
    sequences <- rle(dat[i,] < drift)
    bouts[[i]] <- sequences
  }
  return(bouts)
}
GetBouts <- function(files, act){
  
  dat <- act
  rbouts <- CountRest(files)
  dat$time <- round(seq(from=0, to=48, length.out=240), 2)
  dat$bouts <- 0
  for(j in 1:length(rbouts)){
    foo <- rbouts[[j]]
    point.sb <- (cumsum(foo[[1]]) * 1.06666667)[which(foo[[1]] > 120 & foo[[2]])]
    point.sb <- point.sb/60/60
    for(i in 2:nrow(dat)){
      sleeps <- sum(point.sb < dat$time[i] & point.sb > dat$time[i-1])
      if(sleeps > 0){
        dat$bouts[i] <- sleeps + dat$bouts[i]
      }
    }
  }
  return(dat)
}
GetTimed <- function(dat, reclen = 48, window = 0.2) {

  df <- data.frame(matrix(NA, nrow = length(dat[,1]), ncol = 3))
  colnames(df) <- c("day", "twilight", "night")
  for (i in 1:length(dat[,1])) {
    df[i,1] <- mean(dat[i,c(1:45,116:165,236:240)])
    df[i,2] <- mean(dat[i,c(46:55,106:115,166:175,226:235)])
    df[i,3] <- mean(dat[i,c(56:105,176:225)])
  }
  return(df)
}
Crepuscularity <- function(dat, strain, reclen = 48, window = 0.2 ) {
  
  df <- data.frame(matrix(NA, nrow = length(dat[,1]), ncol = 4))
  colnames(df) <- c("day", "twilight", "night", "strain")
  
  for (i in 1:length(dat[,1])) {
    df[i,1] <- sum(dat[i,c(1:45,116:165,236:240)])/sum(dat[i,])
    df[i,2] <- sum(dat[i,c(46:55,106:115,166:175,226:235)])/sum(dat[i,])
    df[i,3] <- sum(dat[i,c(56:105,176:225)])/sum(dat[i,])
  }
  df[,4] <- strain
  
  return(df)
}

### Data Analysis ###

# Yellow Plakat
yp.csv <- c("data/YP-01-02-mar31DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "data/YP-03-04-apr04DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "data/YP-05-06-apr07DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "data/Yp-07-08-apr11DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")
yp.dat <- GetData(yp.csv)
yp.act <- GetAct(yp.dat)
yp.rest <- GetBouts(yp.csv, yp.act)
yp.timed <- GetTimed(yp.dat)
yp.score <- Crepuscularity(yp.dat, "Yellow Plakat")

#Super Red
sr.csv <- c("data/SR-01-02-May23DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "data/SR-03-04-May26DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
            "data/SR-05-06-May30DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")
sr.dat <- GetData(sr.csv)
sr.act <- GetAct(sr.dat)
sr.rest <- GetBouts(sr.csv, sr.act)
sr.timed <- GetTimed(sr.dat)
sr.score <- Crepuscularity(sr.dat, "Super Red")

# Wild-Type (Betta Splendens)
wtbs.csv <- c("data/WT-BS-01-02-may19DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
              "data/WT-BS-03-04-apr18DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
              "data/WT-BS-05-06-apr21DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv",
              "data/WT-BS-07-08-apr28DLC_dlcrnetms5_yp-wtMay2shuffle1_80000_el.csv")
wtbs.dat <- GetData(wtbs.csv)
wtbs.act <- GetAct(wtbs.dat)
wtbs.rest <- GetBouts(wtbs.csv, wtbs.act)
wtbs.timed <- GetTimed(wtbs.dat)
wtbs.score <- Crepuscularity(wtbs.dat, "Wild-Type")


