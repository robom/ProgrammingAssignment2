## Function makeCacheMatrix takes a matrix and makes its cachable version, 
## so that every time cacheSolve is called, it firstly checks 
## if the result is already cached and computes the matrix inverse only if not.

## Creates a special "matrix" object that can cache its inverse.
## It provides the following functions for manipulation with the object:
## set: sets the new matrix to be cachable
## get: gets the currently stored matrix
## setinverse: caches the inverse of the stored matrix
## getinverse: retrieves the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
