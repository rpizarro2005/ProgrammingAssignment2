## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. These pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## First we fix the result we want (inverse) to "NULL" in the global environment, just to
## be sure we "clear" any previous value in it
        inverse <- NULL
## In the cache (parent environment): 1) first, we make a function to assign
## the "cache value" to the matrix we are going to compute; 2) second, we fix 
## the result we want (inverse) to "NULL" in the parent environment, just to
## be sure we "clear" any previous value in it
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
## We get to the global environment the matrix we want to compute
        get <- function() x
## In the cache (parent environment): we compute
## a "cache value" of the inverse of the matrix
        setinverse <- function(inverse) inverse <<- solve(x)
## We get to the global environment the inverse value; and we create the List, 
## that special "matrix" object...
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
