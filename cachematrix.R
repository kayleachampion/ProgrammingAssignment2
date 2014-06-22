## 6/21/2014 
## 
## These functions allow caching of a matrix inversion computation.
## Inversion can take time to calculate, and by storing the inverse,
## we have it pre-calculated for easy reference to it at a later point.

## makeCacheMatrix()
##   * initializes a blank matrix if one is not provided
##   * returns a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  setMyMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMyMatrix <- function() m
  setMyInverse <- function(inverse) m <<- solve
  getMyInverse <- function() m
  list( ## now return our "matrix" - i.e. list of function pointers
    setMyMatrix = setMyMatrix, 
    getMyMatrix = getMyMatrix, 
    setMyInverse = setMyInverse, 
    getMyInverse = getMyInverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##from example
  m <- x$getMyMatrix()
  if(!is.null(m)) { ##inverse is already present, just return it
    message("getting cached matrix")
    return(m)
  }
  ## no inverse, so we calculate it
  data <- x$getMyMatrix()
  m <- solve(data, ...)
  x$setMyInverse(m) ## store it so we won't need to calculate again
  m ## and return it
}
