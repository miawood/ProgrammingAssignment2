## A pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached inverse")
        return (i)
    }
    matrixdata <- x$get()
    i <- solve(matrixdata, ...)
    x$setinverse(i)
    i
}
