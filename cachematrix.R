## These functions catched the inverse of a matrix rather than computing it repeatedly
## because matrix inversion is a costly computation.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of a matrix returned by makeCacheMatrix function.
## If the inverse han already been calculated (and the matrix has not changed), then
## it should retrieve the inverse from the cache
cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m ## Return a matrix that is the inverse of 'x'
}