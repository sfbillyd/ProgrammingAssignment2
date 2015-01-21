## Put comments here that give an overall description of what your
## functions do

## The point of these functions is to create the inverse of a passed matrix and cache it in a
## separate environment such that it can be retrieved later without wading through the chaff in the
## standard, global environment

## Write a short comment describing this function
## function inverts a passed matrix via the built-in R solve() function it then caches the inverted matrix
## for later retrieval

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- minverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## this function first checks if a cached version of the inverse of the passed matrix exists
## if so then it retrieves the cached inverted matrix
## if not the makeCacheMatrix function will invert the matrix and cache it for later retrieval
## finally, it returns the inverted matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- minverse(data, ...)
    x$setinverse(m)
    m
}
