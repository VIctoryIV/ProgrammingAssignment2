## The function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse.
## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix, which is called n1 in the current script and is got
## due to function solve()

makeCacheMatrix <- function(x = matrix()) {
  n1 <- NULL
  set <- function(y) {
    x <<- y
    n1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n1 <<- inverse
  getinverse <- function() n1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  n1 <- x$getinverse()
  if(!is.null(n1)) {
    message("getting cached data")
    return(n1)
  }
  data <- x$get()
  n1 <- solve(data, ...)
  x$setinverse(n1)
  n1
}