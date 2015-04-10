corr <- function(directory, threshold = 0) {
  corrVal <- numeric(0)
  compObs <- complete("specdata",1:332)
  compObs <- compObs[compObs$nobs > threshold, ]
  
  for (cid in compObs$id) {
    
    readFile <- mon(cid, directory)
    corrVal <- c(corrVal, cor(readFile$sulfate, readFile$nitrate, use = "pairwise.complete.obs"))
  }
  return(corrVal)
}