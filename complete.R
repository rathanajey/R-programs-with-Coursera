complete<-function(directory = "specdata", id = 1:332){
  
  ## read all files in the directory
  fileList<-list.files(directory)
  ## read only required files files 
  fileListID<-fileList[id]
  bindr<-data.frame(id=numeric(), countrows=numeric())
    
    for(i in 1:length(fileListID)){
      fd<-paste(directory,"/",fileListID[i],sep="")
      data<-read.csv(fd)
      rowCt<-sum(complete.cases(data))
      bindr<-rbind(bindr,c(id[i],rowCt))
    }
  
  colnames(bindr)<-c("id","nobs")
  bindr
}
