## Module contains functions: "makeCacheMatrix" which makes a matrix-like list
## caching the inversion process and "cacheSolve" which uses this feature
## by either retriving the inverse or calculating it if not accessible

## Takes x as a matrix input and returns a list emulating a matrix with
## the possibility to cache the inverse via cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## takes a CacheMatrix list and retrives the inverse by either accessing the
## cache or calculating it, returns a matrix type, assuming x is a square matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
          return(inv)
          }
        matr <-x$get()
        inv <- solve(matr, ...)
        x$setinv(inv)
        inv
}
