## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInv <- NULL
  set <- function(y){
    x <<- y
    myInv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) myInv <<- solveMatrix
  getInverse <- function() myInv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MyInv <- x$getInverse()
  
  if(!is.null(myInv)){
    message("Getting data.")
    return(myInv)
  }
  data <- x$get()
  myInv <- solve(data)
  x$setInverse(myInv)
  myInv      
}
