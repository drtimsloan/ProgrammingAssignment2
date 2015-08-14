## These functions make use of lexical scoping in R to store the inverse of a matrix
## and recall it without recalculating if already cached. Begin by calling makeCacheMatrix
## on the matrix to be inverted and assigning it to an object, e.g. x, then call cacheSolve 
## on the assigned object. A new value for the matrix can be assigned by calling x$set()

## makeCacheMatrix is a list of four functions to set and recall the values of the matrix x
## and inverse matrix i

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks to see if the inverse of the matrix x is already
## cached and calculates it if not

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
