  rankall<-function(outcome, num){
    hospData<-read.csv("cera3\\outcome-of-care-measures.csv",colClasses="character")
    outcome<-gsub("^\\s+|\\s+$","",outcome)
    outcome<-gsub(" ",".",outcome)
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
    stateList<-unique(hospData["State"])
    stateList<-stateList[order(stateList["State"]),]
    bestHosps<-data.frame(hospital=character(),state=character(), stringsAsFactors=FALSE)
    for(i in 1:length(stateList))
    {
      stateNum<-num
      state<-stateList[i]
      stateData<-subset(hospData,State==state, select=c(2,outCol))
      for(j in 1:nrow(stateData))
      {
        if(stateData[j,2]=="Not Available")
        {
          stateData[j,2]<-NA 
        }
      }
      stateData<-stateData[complete.cases(stateData),]
      stateData[,2]<-as.numeric(stateData[,2])
      if(num=="best") stateNum<-1
      if(num=="worst") stateNum<-nrow(stateData)
      if(stateNum>nrow(stateData))
      {
        bestHosps[i,1]<-NA
      }
      else
      {
        stateDataBest<-stateData[order(stateData[names(stateData[2])],stateData[names(stateData[1])]),]
        stateDataBest["Rank"]<-c(1:nrow(stateDataBest))
        bestHosps[i,1]<-stateDataBest[stateNum,1]      
      }
      bestHosps[i,2]<-state
    }
  bestHosps
  }
