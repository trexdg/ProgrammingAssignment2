## Functions that can cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the cached inverse is available, cacheSolve retrieves it, and if not, it computes, caches, and returns it

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting reverse matrix...")
    return(inverse)
  }
  data <- mtx$get()
  inverse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
