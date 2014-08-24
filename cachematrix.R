## These functions calculate and cache the inverse of a matrix. The inverse is 
## calculated once using cacheSolve, every subsequent call of the same cacheMatrix
## object will then extract the cached inverse.

## this function creates a cache which holds the original matrix, and the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inv.mat) {
                inv <<- inv.mat
        }
        getinv <- function() {
                inv
        }
        list(set = set, get = get, setinv = setinv, 
             getinv = getinv)
}



## This function calculates the inverse of the given matrix, and stores it in the 
## cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
