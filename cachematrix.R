## makeCacheMatrix caches the square matrix that we assign to x
## cacheSolve caches the inverse of the matrix x

## Take the matrix x as an argument. Sets the value of the matrix. Gets the 
## value of the matrix. Sets the value of the inverse of the matrix. Gets
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setreverse <- function(solve) m <<- solve
    getreverse <- function() m
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
}


## Calculate the inverse of the matrix created with makeCacheMatrix. If the inverse
## was already created with makeCacheMatrix. If it has been, it pulls from the
## cache. If not, calculate the inverse of the matrix and sets the inverse via
## setreverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getreverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setreverse(m)
    m
}
