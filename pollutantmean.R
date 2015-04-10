pollutantmean<-function(directory, pollutant, id)
{
  allSpecData<-list.files(directory)
  pollutantDataFiles<-allSpecData[id]
  pollutantDframe<-NULL
  polData<-NULL
  for(i in 1:length(pollutantDataFiles))
  {
    fileDir<-NULL
    fileDir<-paste(directory,"\\",pollutantDataFiles[i],sep="")
    newData<-read.csv(fileDir)
    polData<-rbind(polData, newData)
  }
  polData<-na.omit(polData)
  if(pollutant=="sulfate") {
    result<-mean(polData$sulfate)
    }
  if(pollutant=="nitrate") {
  result<-mean(polData$nitrate)
    }
  result
}
