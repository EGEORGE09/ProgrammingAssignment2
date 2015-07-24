## The following pair of functions are designed for caching the inverse of a matrix.
## The first function creates a special "matrix" object that can cache its inverse
## The second function returns the inverse of the special "matrix" returned by the first function,
## if the matrix has not changed (hence inverse already calculate and stored in cache) then this is returned
## otherwise the inverse of the matrix is calculated and set in the cache.

## The makeCacheMatrix function creates a special "matrix" by:
## setting and getting the value of the matrix 
## and setting and getting the value of the inverse.
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

## The cacheSolve function calculates the inverse of the special "matrix" created by makeCacheMatrix
## it first checks to see if the inverse has been calculated, if so it gets this value from the cache
## and skips computation.
## If not, the inverse of the matrix is calculated and is set in the cache using the setinverse function.
cacheSolve <- function(x, ...) {
     i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i 
}
