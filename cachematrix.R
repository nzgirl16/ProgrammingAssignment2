## Caching the Inverse of a Matrix


## Creates a special matrix that is used to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setINV <- function(solve) m <<- solve
    getINV <- function() m
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)
}


## Gets the inverse of the matrix by checking first if it has already been
## calculated. If it has, then it will use the stored inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getINV()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setINV(m)
    m
}
