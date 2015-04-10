rankhospital<-function(state, outcome, num){
  hospData<-read.csv("cera3\\outcome-of-care-measures.csv",colClasses="character")
  state<-gsub("^\\s+|\\s+$","",state)
  state<-toupper(state)
  outcome<-gsub("^\\s+|\\s+$","",outcome)
  outcome<-gsub(" ",".",outcome)
  if(any(unique(hospData["State"])==state)==FALSE)
    stop("invalid state")
  attaTest=grep(outcome,names(hospData[11]), ignore.case=TRUE)
  failTest=grep(outcome,names(hospData[17]), ignore.case=TRUE)
  pneuTest=grep(outcome,names(hospData[23]), ignore.case=TRUE)
  if(any(grep(outcome,names(hospData), ignore.case=TRUE))==FALSE)
    stop("invalid outcome")
  if(length(failTest)!=0)
    outCol=17
  if(length(attaTest)!=0)
    outCol=11 
  if(length(pneuTest)!=0)
    outCol=23
  stateData<-subset(hospData,State==state, select=c(2,outCol))
  for(i in 1:nrow(stateData))
  {
    if(stateData[i,2]=="Not Available")
    {
      stateData[i,2]<-NA 
    }
  }
  stateData<-stateData[complete.cases(stateData),]
  stateData[,2]<-as.numeric(stateData[,2])
  stateDataBest<-stateData[order(stateData[names(stateData[2])],stateData[names(stateData[1])]),]
  stateDataBest["Rank"]<-c(1:nrow(stateDataBest))
  if(num=="best")
  {
    num<-1
  }
  if(num=="worst")
  {
    num<- nrow(stateData)
  }
  if(num>nrow(stateData))
  {
    return(NA)
  }
  stateDataBest[num,1]
}
