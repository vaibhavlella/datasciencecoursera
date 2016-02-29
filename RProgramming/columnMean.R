columnMean <- function(y,removeNA = TRUE){
  nc<-ncol(y)
  mn<-numeric(nc)
  for(i in 1:nc){
    mn[i]<-mean(y[,i],na.rm=removeNA)
  }
   mn;
}
  