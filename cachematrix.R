## The following functions are used to create and use an object
## that caches the inverse of a matrix.

## Returns an object that wraps the received matrix, and also caches its
## inverse. This object supports methods `get` and `set` to get/set the wrapped
## matrix, and supports also methods `getinverse` and `setinverse` to get/set
## the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returns the inverse of matrix wrapped in the received object, computing it if
## not already in cache. The ... arguments are passed to the `solve` function,
## which is used to compute the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
