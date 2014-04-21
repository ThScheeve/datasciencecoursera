## makeCacheMatrix: Creates a special "matrix" object that can cache its 
## inverse.
##
## cacheSolve: Computes the inverse of the special "matrix" object returned by 
## makeCacheMatrix. If the inverse has already been calculated, the inverse is 
## retrieved from the cache by the getInverse function and computation is 
## skipped. Otherwise, the inverse of the matrix is calculated and the value of 
## the inverse is set in the cache via the setInverse function.


## Creates a special "matrix" object that can cache its inverse.
##
## Args:
##   x: Matrix object whose inverse is to be calculated.
##
## Returns:
##   The special "matrix" object; a list containing a function to 
##     1) set the value of the matrix
##     2) get the value of the matrix
##     3) set the value of the inverse
##     4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" object returned by 
## makeCacheMatrix above.
##
## Args:
##   x: Special "matrix" object whose inverse is to be calculated.
##   ...: Further arguments passed to or from other methods.
##
## Returns:
##   The matrix that is the inverse of x.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
