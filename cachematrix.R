# source("cachematrix.R")-
## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse with three functions for cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  # initilizing matrix inverse
  inv <- NULL
  
  # with 'get' you get the data, i.e. matrix x
  get <- function() x
  
  # set the inverse as the value given to function
  setInverse <- function(inverse) inv <<- inverse
  
  # this returns the inverse
  getInverse <- function() inv
  
  # this function returns list of three functions that are usable outside of the function
  list(get = get, setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## First checking if the inverse has been calculated
  inv <- x$getInverse()
  if(!is.null(inv)) {
    ## in case Inverse wasn't at it initial value (NULL) using cached data from
    ## makeCacheMatrix
    message("getting cached data")
    return(inv)
  }
  ## Otherwise cacheSolve needs to calculate the inverse
  data <- x$get()
  ## Notice solve() works only with square matrices
  inv <- solve(data, ...)
  ## Finally sets the inverse in cache for makeCacheMatrix
  x$setInverse(inv)
  inv
}
