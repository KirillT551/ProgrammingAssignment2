## Put comments here that give an overall description of what your
## functions do
##This function solves the inverse of a matrix and store the answer in a cache.
##This is done to reduce the computing time of recurring calculations.

## Write a short comment describing this function
##This function provides a list of functions were you configure/solve your matrix.
##Function
##  -get: Shows the matrix to be solved.
##  -setInv: Set the matrix to be solved.
##  -getInv: Retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
##This function solves the inverse of the matrix. This solution (inverse)
##can be achieve by solving the matrix or searching the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

