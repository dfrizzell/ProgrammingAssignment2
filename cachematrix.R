## Functions for caching the inverse of a matrix. makeCacheMatrix sets up and initializes
## a matrix. cacheSolve will check if an inverse was cached, it will return the matrix or
## compute the inverse


## makeCacheMatrix - initializes an empty matrix or accepts a matrix to initialze. Returns
## a list that has get and set methods

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}


## cacheSolve - checks if an inverse matrix is cached, returns the inverse if possible. If
## no cached inverse, then solve will be used to compute the inverse.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
