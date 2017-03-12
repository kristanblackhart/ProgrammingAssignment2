## This is a two-step process to cache the inverser of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    cinv <- NULL
    set <- function(y) {
        m <<- y
        cinv <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) cinv <<- inverse
    getInverse <- function() cinv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse is already available (with no changes 
## to the matrix), this function will retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
    inverse <- m$getInverse()
    if(!is.null(cinv)){
        message("getting cached data")
        return(cinv)
    }
    data <- m$get()
    cinv <- solve(data)
    m$setInverse(cinv)
    cinv
}
