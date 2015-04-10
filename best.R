best<-function(state,outcome){
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
  stateData[,2]<-as.numeric(stateData[,2])
  minOut=min(stateData[,2],na.rm=TRUE)
  stateDataBest<-subset(stateData,stateData[,2]==minOut)
  stateDataBest<-stateDataBest[order(stateDataBest["Hospital.Name"]),]
  stateDataBest[1,1]
}
