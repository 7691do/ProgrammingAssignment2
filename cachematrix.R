# Implements a special matrix object that can cache it's inverse
# in order to avoid the recalculation of the costly inverse operation
# as long as the matrix did not change since the last calculation.


# Creates a special matrix object that contains a standard matrix,
# its cached inverse or NULL if not yet calculated,
# and getters and setters for both the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Gets the cached inversed of the matrix if available, or calculates
# the inverse and store it in the cache if not available.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
