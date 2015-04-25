## This R script calculates the inverse of a matrix but only when it hasn't been calculated 
##already. It has two parts:
##1) makeCacheMatrix
##2) cacheSolve

## 'makeCacheMatrix' produces a list of functions that, a) initialize the matrix and inverse, b) obtain the matrix,
## c) sets/calculates the inverse, d) gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invers <<- solve
  getinv <- function() invers
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse if it hasn't already been calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invers <- x$getinv()
    if(!is.null(invers)) {
      message("getting cached inverse")
      return(invers)
    }
    data <- x$get()
    invers <- solve(data, ...)
    x$setinv(invers)
    invers  
}
