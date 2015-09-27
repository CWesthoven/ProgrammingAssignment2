## This is a set of fucntions desined to cache a matrix
## and return it's inverse if there has not been a change.


## This function creates a matrix object that can cache it's inverse.
## It is used as the input to the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix created by the makeCacheMatrix
## function above. In the case when the inverse has already been calculated and the 
## result hasn't changed the cached value will simply be returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    iv <- x$getinverse()
    ## If the cached value exists return it, otherwise calculate and cache the result
    if(!is.null(iv)) {
      message("getting cached data")
      return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    iv
  }
