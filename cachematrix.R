## This set of functions calculate and cache the inverse of a matrix. It
## assumes that the supplied matrix is invertible and will look up the cached
## inverse rather than recomputing for repeated computations. A special object
## stores a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix" allowing you to
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse
##     4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with
## makeCacheMatrix. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
