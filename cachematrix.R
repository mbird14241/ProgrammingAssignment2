## This file implements some helper functions for dealing
## with matrices andf caching their inverses

## Important: if the user passed additional arguments to cacheSolve() and
## then called it again with other additional arguments, it would return
## the original cached matrix, not one calculated with with the second 
## set of additional arguments. That would be a bug.


## This function "makes" a matrix capable of caching its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Cache the last set of arguments in addition to the last inverse 
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
    
    ## if there is a matrix and the additional arguments 
    ## passed last time match, return the cached inverse.
    ## Otherwise recompute.
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
    
    ## Also, cache the args that we calculated with.
    x$setLastArgs(thisArgs)
    i
}


