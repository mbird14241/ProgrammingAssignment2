## This file implements some helper functions for dealing
## with matrices andf caching their inverses

## This function "makes" a matrix capable of caching its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function returns the cached matrix inverse if 
## it exists, calculates it otherwise.

cacheSolve <- function(x) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached matrix inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
