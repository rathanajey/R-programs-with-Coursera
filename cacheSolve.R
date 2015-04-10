cacheSolve <- function(inpMatrix, ...) {
  
  inverseMatrix <- inpMatrix$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  tempInpMatrix <- inpMatrix$getStoredMatrix()
  inpInverse <- solve(tempInpMatrix)
  inpMatrix$setInverse(inpInverse)
  inpInverse
}