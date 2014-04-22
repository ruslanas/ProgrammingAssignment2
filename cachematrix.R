## Matrix inversion with caching

## Create ivertible matrix object

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL # Init inverse
    
    set <- function(m) {
        x <<- m
        inv <<- NULL # Reset inverse
    }
    
    get <- function() {
        return(x)
    }
    
    setInverse <- function(m) {
        inv <<- m
        return(m)
    }
    
    getInverse <- function() {
        return(inv)
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Find inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("Found in cache")
    } else {
        inv <- x$setInverse(solve(x$get()))
    }
    
    return(inv)    
}
