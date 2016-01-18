## This pair of functions cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) s<<- solve
  getmatrix <- function() s
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of 'makeCacheMatrix'. If the inverse 
## has already been calculated then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getmatrix()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setmatrix(s)
  s
}
