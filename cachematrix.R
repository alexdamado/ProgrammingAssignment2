## The functions will cache results of the inverse matrix saving time in future calculations by using the saved values instead of
## calculating everything once again.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  setinv <- function (y){
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  list(setinv = setinv, getmatrix = getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##     the inverse from the cache.

cachesolve <- function(x, ...) {
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinv(inv)
  inv
}
