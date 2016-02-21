## This file implements some helper functions for dealing
## with matrices andf caching their inverses

## This function "makes" a matrix capable of caching its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    lastArgs <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        lastArgs <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    setLastArgs <- function(la) lastArgs <<- la
    getLastArgs <- function() lastArgs
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         getLastArgs = getLastArgs,
         setLastArgs = setLastArgs)
    
}


## This function returns the cached matrix inverse if 
## it exists, calculates it otherwise.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    lastArgs <- x$getLastArgs()
    thisArgs <- c(...)
    if(!is.null(i) && 
       length(thisArgs) == length(lastArgs) &&
       all(thisArgs == lastArgs))
    {
        message("getting cached matrix inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    x$setLastArgs(thisArgs)
    i
}
