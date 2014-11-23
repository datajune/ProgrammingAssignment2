## These functions will calculate the inverse of a square matrix and
## store the result in cache.

## The makeCacheMatrix creates a list of 4 functions - set, get, setinverse
## and get inverse - for using when solving for the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function uses the functions defined in makeCacheMatrix
## to calculate the inverse of a square matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}