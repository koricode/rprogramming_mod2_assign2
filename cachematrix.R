## Put comments here that give an overall description of what your
## functions do

## This function converts a matrix into a list with functions to manipulate
## the matrix itself and cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    # Nulls the matrix inverse as it was not calculated yet
    i <- NULL
    
    # Function to set the internal matrix
    set <- function(v) {
        # Stop and trigger an error if the argument is not a matrix
        if (class(v) != "matrix") {
            stop("the argument is not a matrix")
        }
        
        m <<- v
        i <<- NULL
    }
    
    # Function to get the internal matrix
    get <- function() {
        m
    }
    
    # Function to set the internal matrix inverse
    setInverse <- function(v) {
        i <<- v
    }
    
    # Function to get the internal matrix inverse
    getInverse <- function() {
        i
    }
    
    # Sets the internal matrix with the one provided during object creation
    set(m)
    
    list(ltype = "cache-matrix", set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of a cache matrix. First, it looks if the
## matrix already contains a cached inverse. If it contains, returns it. If not,
## solves the matrix and stores its inverse into the cache.

cacheSolve <- function(m, ...) {
    if (!is.list(m) || is.null(m$ltype)) {
        stop("the argument is not a cache matrix")
    }
    
    ## Return a matrix that is the inverse of 'm'
    i <- m$getInverse()
    
    if (is.null(i)) {
        ## Cache is empty
        ## Solve the matrix, store the result in the cache and returns it
        data <- m$get()
        i <- solve(data, ...)
        m$setInverse(i)
    } else {
        ## Cache is NOT empty
        ## Print some info to the user and returns the cached result
        message("getting cached data")
    }

    i
}
