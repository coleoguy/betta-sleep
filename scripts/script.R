# Andres Barboza
# 03/27/2023

library(tidyr)

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

data <- read.csv("../data/plakats-24-15fpsDLC_dlcrnetms5_plakat-trackingMar8shuffle1_50000_el.csv",
                 header=T, na.strings=c(""," ","NA"))[-c(1:7),c(2:4,32:34)]

b1 <- data[,1:3]
b2 <- data[,1:3]
data[,1] <- as.numeric(data[,1])
data[,2] <- as.numeric(data[,2])

plot(GetData(x, reclen = 24, window = .10, stat="sum"))
  
  
  
