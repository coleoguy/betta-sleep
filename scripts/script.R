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
GetData <- function(files){
  
  dat <- matrix(NA, (length(files)*2), 48)
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
    dat[counter, 1:48] <- GetMovement(b1, reclen = 48, window = 1, stat="sum")
    counter <- counter + 1
    dat[counter, 1:48] <- GetMovement(b2, reclen = 48, window = 1, stat="sum")
  }
  
  upper <- lower <- c()
  for(i in 1:48){
    x <- t.test(dat[,i])$conf.int
    lower[i] <- x[1]
    upper[i] <- x[2]
  }
  df <- data.frame(colMeans(dat), lower, upper)
  colnames(df)[1] <- "activity"
  
  return(df)
}


csv.names <- c("../data/YP-01-02-mar31DLC_dlcrnetms5_plakat-trackingMar8shuffle1_50000_el.csv",
               "../data/YP-03-04-apr04DLC_dlcrnetms5_plakat-trackingMar8shuffle1_50000_el.csv")


df <- GetData(csv.names)


# Plotting - Requires df of means, lower, upper columns
ggtheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              panel.border=element_blank(),
                              axis.line = element_line(colour="grey30"),
                              axis.title = element_text(colour="grey20"),
                              axis.text = (element_text(colour="grey30")),
                              legend.title = element_text(colour="grey20"),
                              legend.text = element_text(colour="grey30"))

ggplot(df, aes(x = 1:48, y = activity)) + 
  ggtheme +
  geom_line(col="black") + 
  geom_point() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha=0.1, 
              linetype="dashed",
              color="grey") +
  annotate("rect", xmin = 10, xmax = 22, ymin = -1, ymax = 70000,
           alpha = .2,fill = "gray5") +
  annotate("rect", xmin = 34, xmax = 46, ymin = -1, ymax = 70000,
           alpha = .2,fill = "gray5")

