## These functions allows to reuse the inverse of a matrix without
## computed each time is need it. You must use 'makeCacheMatrix'
## to generate a cache version of your matrix the first time. After,
## you can compute or get the inverse using 'cacheSolve'. If you need 
## the inverse of a new matrix, you can use the set function of 
## cached matrix in order to update the matrix value.

## Generate a list of functions that can be used to caching a matrix and its 
## inverse, in order to avoid the inverse computation of the same matrix
## multiple times.

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is the matrix which inverse must be cached.

        ## Return a list of functions that can be used to caching a matrix 
        ## and its inverse.
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invers) inv <<- invers
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of a matrix if it has not been computed before. 
## Or just return the inverse value in cache if this was already computed.

cacheSolve <- function(x, ...) {
        ## 'x' is a vector generaed with 'makeCacheMatrix' and represents
        ## the a cached version of the matrix whcih inverse we want to compute.
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
