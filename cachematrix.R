## Together these functions 1) create a special object that stores an invertible
## matrix and caches its inverse

## The first function creates a special object that is a list of functions
## that can set and retreive a matrix and its inverse. It does this by using 
## the <<- operator to assign a value to an object in an environment that is
## not the current environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function calculates the inverse of the matrix given. It first
## checks whether the inverse has already been calculated; if so, it gets 
## the inverse from the cache and does not run the computation. Otherwise, 
## it runs the inverse function "solve" and sets that in the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
