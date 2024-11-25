## This script creates two functions that take advantage of the 
## scoping rules of R, for the Week 2 assignment
## functions do

## makeCacheMatrix creates a list of functions 
## associated with a matrix, to save on computing power
## and holds the matrix in cache memory for using later

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
}


## cacheSolve uses the cached matrix created earlier and 
## performs the inverse function on the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
