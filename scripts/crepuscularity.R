
## TODO get data to results then read in the results csv with data labeled for time periods

Crepuscularity <- function(act, reclen = 48, window = 0.2 ) {
  crep.act <- sum(yp.dnt$movement[yp.dnt$time == "twilight"])
  diu.act <- sum(yp.dnt$movement[yp.dnt$time == "day"])
  noc.act <- sum(yp.dnt$movement[yp.dnt$time == "night"])
  tot.act <- sum(yp.dnt$movement)
  crepuscularity <- crep.act/tot.act
  diurnality <- diu.act/tot.act
  nocturnalitu <- noc.act/tot.act
  
  dat <- GetData(act, reclen, window)
  for (i in length(dat[,1])) {
    
  }
  
  
}