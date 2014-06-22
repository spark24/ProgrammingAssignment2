## This set of functions implement fast matrix inverses for 
## programming assignment 2 for R programming.  
## cacheSolve

## caches matrix inverses with makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinv <- function(solved) inv <<- solved
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Inverts a matrix.  If the matrix inverse already exists
## in cache, uses the stored inverse.  Otherwise calls solve.


cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
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
