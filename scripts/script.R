# Andres Barboza
# 03/27/2023

library(tidyr)
library(ggplot2)

GetData <- function(x, reclen, window, stat){
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

data <- read.csv("../data/YP-01-02-mar31DLC_dlcrnetms5_plakat-trackingMar8shuffle1_50000_el.csv",
                 header=T, na.strings=c(""," ","NA"))[-c(1:7),c(2:4,32:34)]

b1 <- data[,1:3]
b1[,1] <- as.numeric(b1[,1])
b1[,2] <- as.numeric(b1[,2])
b2 <- data[,4:6]
b2[,1] <- as.numeric(b2[,1])
b2[,2] <- as.numeric(b2[,2])

Plot <- function(x){
  plot(GetData(x, reclen = 48, window = 1, stat="sum"), type = 'l', xaxt='n')
  axis(side=1, at=c(0:48))  
  rect(xleft=10,xright =22,ybottom=0,ytop=70000, density=NA, col = rgb(0.1,0.1,0.1,alpha=0.5))
  rect(xleft=34,xright =46,ybottom=0,ytop=70000, density=NA, col = rgb(0.1,0.1,0.1,alpha=0.5))  
}

Plot(b1)


p <- ggplot(data=GetData(b1, reclen = 48, window = 1, stat="sum"), aes(x=interval, y=OR, colour=Drug)) + geom_point() + geom_line()
p <- p+geom_ribbon(aes(ymin=data$lower, ymax=data$upper), linetype=2, alpha=0.1)