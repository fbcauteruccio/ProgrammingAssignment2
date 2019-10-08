## The objective is to calculate the inverse of a
## matrix, and then store it´s value in cache.
## So the computing will be done only once

#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mcInverse <- NULL
        set <- function(y) {
                x <<- y
                mcInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mcInverse <<- inverse
        getInverse <- function() mcInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        mInverse <- x$getInverse()
        if(!is.null(mInverse)) {
                message("getting cached data")
                return(mInverse)
        }
        data <- x$get()
        mInverse <- solve(data, ...)
        x$setInverse(mInverse)
        mInverse
}
