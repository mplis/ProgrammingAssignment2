## Functions that allow for the caching of the inverse of 
## a matrix to avoid expensive, redundant calculations

# Creates object to store a matrix and cache the value of 
# its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  list(get=get,
       set=set,
       getinverse=getinverse,
       setinverse=setinverse)
}

# Returns the inverse of the matrix if it's cached, or 
# calculates the inverse if no cached value exists

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
