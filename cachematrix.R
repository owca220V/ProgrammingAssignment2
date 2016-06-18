## This code is part of the R Programming Assignment2: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## this (m) will be our cache
  m <- NULL

  ## define the set, get, setInverste and getInverse functions
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  ## look up the buffered inverse
  m <- x$getInverse()
  
  ## if it's there (the inverse has been computed previously)
  if(!is.null(m)){
    ## let the user know you're accessing cache
    message("getting cached data")
    ## and return the cached value (m)
    return(m)
  }
  ## otherwsie, pull up the data and calculate the inverse
  data <- x$get()
  m <- solve(data,...)
  
  ## save / cache the result for future reference
  x$setInverse(m)
  
  ## return the computed (and cached) value m
  m
}
