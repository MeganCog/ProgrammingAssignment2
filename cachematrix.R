## This file contains two functions: makeCacheMatrix, which
## creates a special "matrix" object that can cache its inverse,
## and cacheSolve, which computes the inverse of the
## special "matrix"

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
  }
    get <- function() x
    solveMat <- function(solve) m <<- solve
    getSol <- function() m
  
    list(set = set, get = get,
            solveMat = solveMat,
            getSol = getSol)
}

## The cacheSolve function computes the inverse
## of the special "matrix" object

cacheSolve <- function(x, ...) {
    m <- x$getSol()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
    data <- x$get()
  
  m <- solve(data, ...)
  
  ## Return a matrix that is the inverse of 'x'
  
  x$solveMat(m)
}
