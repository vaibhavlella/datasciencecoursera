corr <- function(directory,threshold = 0) {
  data <- numeric()
  for (i in 1:332) {
    monitor <- read.csv(paste(directory,"/",formatC(i,flag = "0",width = 3),".csv",sep = ""))
    compl <- complete.cases(monitor)
    if (sum(complete.cases(monitor)) > threshold) {
      data <- c(data,cor(monitor[compl,]$sulfate,monitor[compl,]$nitrate))
      
    }
    else{
      data <- c(data,numeric())
    }
  }
  data
}
