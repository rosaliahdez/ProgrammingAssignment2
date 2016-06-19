## This script can be used to cache the inverse matrix of an invertible one. 

## This first function creates a special matrix, which actually is 
## a list of 4 functions, namely: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the subyacent matrix associated
## to makeCacheMatrix, checking in first place if the inverse has been already 
## calculated. If this inverse is cached in memory, it is printed out. 
## Otherwise, it calculates and prints the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
