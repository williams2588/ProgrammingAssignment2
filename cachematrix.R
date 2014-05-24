## This file defines two functions designed to retrieve
## a cached inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        seeinvm <- function(mean) invm <<- mean
        getinvm <- function() m
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        invm <- x$getinvm()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinvm(invm)
        invm
}
