makeCacheMatrix <- function(inpMatrix=matrix()) {
  inverseMatrix <- NULL
  setStoredMatrix <- function(tempInpMatrix) {
    storedMatrix <<- tempInpMatrix
    inverseMatrix <<- NULL
  }
  getStoredMatrix <- function() storedMatrix
  setInverse <- function(inpInverse) inverseMatrix <<- inpInverse
  getInverse <- function() inverseMatrix
  list(setStoredMatrix = setStoredMatrix, getStoredMatrix= getStoredMatrix, setInverse= setInverse, getInverse = getInverse) 
}