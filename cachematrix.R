## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    
    set <- function(v) {
        m <<- v
        i <<- NULL
    }
    
    get <- function() {
        m
    }
    
    setInverse <- function(v) {
        i <<- v
    }
    
    getInverse <- function() {
        i
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- m$getInverse()
    
    if (is.null(i)) {
        data <- m$get()
        i <- solve(data, ...)
        m$setInverse(i)
    } else {
        message("getting cached data")
    }

    i
}
