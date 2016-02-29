# 1)Plot the 30-day mortality rates for heart attack
outcome<- read.csv(file="~/R/Coursera/RProgramming/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])
# 2)Finding the best hospital in a state
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
data1<-subset.data.frame(outcome,select = c(2,7,11,17,23))
data1[,3]<-as.numeric(data1[,3])
data1[,4]<-as.numeric(data1[,4])
data1[,5]<-as.numeric(data1[,5])
colnames(data1)[3]<- "heart attack"
colnames(data1)[4]<- "heart failure"
colnames(data1)[5]<- "pneumonia"

best <- function(state, outcome) {
  if(!state %in% unique(data1$State)){
    print("invalid state")
  }
  else if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    print("invalid outcome")
  }
  else{
  x<- subset(x = data1, data1$State==state,select = c("Hospital.Name",outcome))
  x<- x[complete.cases(x),]
  list<- x[x[[outcome]]==min(x[[outcome]]),1]
  best<- min(list)
  }
  best
}
best("TX","heart attack")

# 3) Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num="best") {
  if(!state %in% unique(data1$State)){
    print("invalid state")
  }
  else if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    print("invalid outcome")
  }
  else{
    x<- subset(x = data1, data1$State==state,select = c("Hospital.Name",outcome))
    x<- x[complete.cases(x),]
    ranked<- x[order(x[[outcome]],x[["Hospital.Name"]]),]
    if(num=="best"){
      rankhospital<- ranked[1,1]
    }
    else if(num=="worst"){
    rankhospital<- ranked[nrow(ranked),1]
    }
    else{
      rankhospital<- ranked[num,1]
      }
  }
  rankhospital
}
# Test Case
rankhospital("TX", "heart failure", 4)

# 4) Ranking hospitals in all states
rankall <- function(outcome, num = "best") {

 if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    print("invalid outcome")
  }
  else{
    x<- subset(x = data1,select = c("Hospital.Name",outcome,"State"))
    x<- x[complete.cases(x),]
    spl<-split(x,x$State)
    z<- lapply(spl,function(x) x[order(x[[outcome]],x[["Hospital.Name"]]),])
    w<- sapply(z,function(x) x[25,])
    if(num=="best"){
      w<- sapply(z,function(x) x[1,])
      rankall<- as.data.frame(t(w)[,c(1,3)])
    }
    else if(num=="worst"){
      w<- sapply(z,function(x) x[nrow(x),])
      rankall<- as.data.frame(t(w)[,c(1,3)])
    }
    else{
      w<- sapply(z,function(x) x[num,])
      rankall<- as.data.frame(t(w)[,c(1,3)])
    }
  }
  rankall
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
# Test Case
head(rankall("heart attack", 20), 10)



