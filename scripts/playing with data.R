
yp_bouts <- readRDS("~/Desktop/betta/yp_bouts.rds")
yp <- read.csv("../results/yp_act.csv")[,-1]
yp$time <- round(seq(from=0, to=48, length.out=240), 2)
yp$bouts <- 0

for(j in 1:length(yp_bouts)){
  foo <- yp_bouts[[j]]
  point.sb <- (cumsum(foo[[1]]) * 1.06666667)[which(foo[[1]] > 60 & foo[[2]])]
  point.sb <- point.sb/60/60
  for(i in 2:nrow(yp)){
    sleeps <- sum(point.sb < yp$time[i] & point.sb > yp$time[i-1])
    if(sleeps > 0){
      yp$bouts[i] <- sleeps + yp$bouts[i]
    }
  }
}

plot(yp$activity~yp$time, type="l", ylim=c(-5,max(yp$activity)))
stdsz <- max(yp$bouts)/1.5
for(i in 1:nrow(yp)){
  points(x=yp$time[i], y=5, cex=yp$bouts[i]/stdsz, pch="|")
}




