complete<- function(directory, id=1:322){
  data <- data.frame()
  for(i in id){
    data<-rbind(data,data.frame(id=i,nobs=sum(complete.cases(read.csv(paste(directory,"/",formatC(i,flag = "0",width = 3),".csv",sep = ""))))))
  }
  data
}