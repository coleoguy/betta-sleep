# Andres Barboza
# 03/27/2023

library(tidyr)

data <- read.csv("../data/plakats-24-15fpsDLC_dlcrnetms5_plakat-trackingMar8shuffle1_50000_el.csv",
                 header=T, na.strings=c(""," ","NA"))
#data <- data[complete.cases(data),]
mouth1 <- data[,c(2,3)]
mouth2 <- data[,c(31,32)]
delta1 <- c()
delta2 <- c()

ColumnFixing <- function(bodyp) {
  bodyp <- bodyp %>% fill(colnames(mouth1[1]), .direction = "up")
  bodyp <- bodyp %>% fill(colnames(mouth1[2]), .direction = "up")
  return(bodyp)
}

Displacement <- function(bodyp, delta) {
  for (i in 4:nrow(bodyp)) {
    x <- as.numeric(bodyp[i,1]) - as.numeric(bodyp[i+1,1])
    y <- as.numeric(bodyp[i,2]) - as.numeric(bodyp[i+1,2])
    d <- sqrt((x^2)+(y^2))
    delta <- append(delta, d)
  }
  return(delta)
}

actlevel <- rep(0, 24)
counter <- 1
starts <- round(seq(from=1, to=1296037, length.out=24))
for(i in starts){
  actlevel[counter] <-  mean(delta1[i]:delta1[(i+2)], na.rm=T)
  counter <- counter + 1
}

plot(x=1:24, y=actlevel, type="l")
plot(y=delta, x=1:980921)
delta[400000:400020]
actlevel>foo
