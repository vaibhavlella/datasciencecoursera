pollutantmean<-function(directory,pollutant,id=1:332)
{
  pmean<-numeric(length = 3)
  data <- data.frame()
  for(i in id){
    data<-rbind(data,read.csv(paste(directory,"/",formatC(i,flag = "0",width = 3),".csv",sep = "")))
  }
    if(pollutant=="sulfate"){
    pmean<-mean(data$sulfate,na.rm = TRUE)
    }
    else if(pollutant=="nitrate")
    {
    pmean<-mean(data$nitrate,na.rm = TRUE) 
    }
    else{
      print("Error")
    }
  round(pmean,digits = 3)
}