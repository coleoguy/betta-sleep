
## TODO get data to results then read in the results csv with data labeled for time periods

Crepuscularity <- function(files, strain, reclen = 48, window = 0.2 ) {
  dat <- GetData(files, reclen, window)
  
  df <- data.frame(matrix(NA, nrow = length(dat[,1]), ncol = 4))
  colnames(df) <- c("day", "twilight", "night", strain)
  
  for (i in 1:length(dat[,1])) {
    df[i,1] <- sum(dat[i,c(1:45,116:165,236:240)])/sum(dat[i,])
    df[i,2] <- sum(dat[i,c(46:55,106:115,166:175,226:235)])/sum(dat[i,])
    df[i,3] <- sum(dat[i,c(56:105,176:225)])/sum(dat[i,])
  }
  df[,4] <- strain
  
  return(df)
}