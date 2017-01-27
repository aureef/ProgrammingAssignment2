## COURSERA - R Programming - Assignement 2
###############################################################################
## Here are a pair of functions that cache the inverse of an invertible matrix.
## 1/ Function makeCacheMatrix creates a special "matrix" object that can cache 
##    its inverse
## 2/ Function cacheSolve returns the inverse matrix in argument and stores it 
##    in the "special matrix" object

###############################################################################
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Assumption : x is invertible
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function () x
  setinv <- function(invers) inv <<- invers
  getinv <- function () inv
  list (set=set, get=get, 
        setinv=setinv, getinv=getinv)
}

###############################################################################
## This function returns a matrix that is the inverse of 'x' and stores it 
## in the "special matrix" object

cacheSolve <- function(x, ...) {
  inv <- x$getinv()     #retrieving the inverse from cached data if exists
  if(!is.null(inv)) {   
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)         #putting the inverse in cached data
  inv
}
