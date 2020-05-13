## The functions below create a cache of the inverse of the input matrix and
## store it for later use. The inverse of the given matrix is calculated for
## the first time. It is retrieved from the cache if the inverse of the same
## matrix is called for again.
## Actually, this is the modified version of the example given in the Coursera
## website.

## The makeCacheMatrix function creates a new vector containing the cache of
## inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function looks for the inverse of the matrix in the cache
## and use it when available. If the cache is not available, the new one is
## calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
