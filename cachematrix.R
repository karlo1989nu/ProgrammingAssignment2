## Functions to cach the inverse of a matrix 

## It creates a matrix and then caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) inv <<- solveMatrix
  getInv <- function() inv 
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Function to compute the inverse of the matrix from makeCacheMatrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv      
}
