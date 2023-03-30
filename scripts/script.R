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
data[,1] <- as.numeric(data[,1])
data[,2] <- as.numeric(data[,2])
x <- data

plot(GetData(x, reclen = 24, window = .10, stat="sum"))
  
  
  














data <- read.csv("../data/plakats-24-15fpsDLC_dlcrnetms5_plakat-trackingMar8shuffle1_50000_el.csv",
                 header=T, na.strings=c(""," ","NA"))[-c(1:7),c(2:4,32:34)]
data[,1] <- as.numeric(data[,1])
data[,2] <- as.numeric(data[,2])
#data <- data[complete.cases(data),]
mouth1 <- data[,c(2,3)]
mouth2 <- data[,c(31,32)]
delta1 <- c()
delta2 <- c()

foo <- data.frame(c(1,2,3,4,5,NA,3,5,NA,8),
                  c(3,4,2,24,NA,23,5,7,8,9))
colnames(foo) <- c("one","two")
foo %>% fill(colnames(foo), .direction="up")

ColumnFixing <- function(bodyp) {
  bodyp <- bodyp %>% fill(colnames(mouth1[1]), .direction = "up")
  bodyp <- bodyp %>% fill(colnames(mouth1[2]), .direction = "up")
  return(bodyp)
}



actlevel <- rep(0, 24)
counter <- 1
for(i in starts){
  actlevel[counter] <-  mean(delta1[i]:delta1[(i+2)], na.rm=T)
  counter <- counter + 1
}

plot(x=1:24, y=actlevel, type="l")
plot(y=delta, x=1:980921)
delta[400000:400020]
actlevel>foo
