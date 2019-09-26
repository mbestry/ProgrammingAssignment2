## The two functions (makeCacheMatrix and cacheSolve) will cache and then retrieve the inverse of matrix object "x" in R

## makeCacheMatrix creates an invertible, square matrix "x" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve retrieves the inverse of the matrix "x"

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
