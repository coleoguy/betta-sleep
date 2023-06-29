
## TODO get data to results then read in the results csv with data labeled for time periods

Crepuscularity <- function(a) {
  crep.act <- sum(yp.dnt$movement[yp.dnt$time == "twilight"])
  diu.act <- sum(yp.dnt$movement[yp.dnt$time == "day"])
  noc.act <- sum(yp.dnt$movement[yp.dnt$time == "night"])
  tot.act <- sum(yp.dnt$movement)
  crepuscularity <- crep.act/tot.act
  diurnality <- diu.act/tot.act
  nocturnalitu <- noc.act/tot.act
}