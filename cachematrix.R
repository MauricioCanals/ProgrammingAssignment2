## Two functions that produce the inverse of a matrix, calculating if it hasn't
## been calculated and bringing the value out of the cache otherwise.

## This first function makes the matrix cache-able, through the definition of
## a list of functions: set, get, setinv and getinv, based on a supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(invert) i <<- invert
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This second function does the calculation of the inverse matrix if it hasn't
## been done, or gets the matrix from the cache if it has. For its argument
## it needs the list of functions created through makeCacheMatrix.

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
  ## Return a matrix that is the inverse of 'x'
}
