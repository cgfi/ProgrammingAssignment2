## With the functions makeCacheMatrix and cacheSolve we can create a matrix and compute it's
## inverse and cache it for later use. If a cached inverse exists the functions
## will retrieve the cached data instead of going through the computations again.

## makeCacheMatrix creates a special matrix or a list that
## includes four functions to set the value of the matrix, get the
## value of the matrix, set the inverse of the matrix and get the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # this makes sure that if a new value is set, the inverse is reset
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of a matrix of the type makeCacheMatrix
## If the inverse has already been computed the function retrieves
## the cached value of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # returns the inverse if it has been cached
  }
  data <- x$get()
  inv <- solve(data, ...) # computing the inverse
  x$setinv(inv)
  inv
}
