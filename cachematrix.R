## The following functions are writen to cache the inverse of a matrix:
## i) makeCacheMatrix: This function creates a special "matrix" object that can
##    cache its inverse.
## ii) cacheSolve: This function computes the inverse of the special "matrix"
##    returned by the function makeCacheMatrix. If the inverse has already been
##    calculated (and the matrix has not changed), then the cachesolve will
##    retrieve the inverse from the cache.


## Short description of the makeCacheMatrix function:
## The function creates a special "matrix", in particular a list containing the 
## following functions:
## i) set: sets the value of the matrix to an input value and its inverse
##    ('invMat') to NULL
## ii) get: gets the value of the matrix
## iii) setinvMat: sets the value of the inverse matrix ('invMat') to an input
##      value ('inverseMatrix')
## iv) getinvMat: gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setinvMat <- function(inverseMatrix) invMat <<- inverseMatrix
    getinvMat <- function() invMat
    list(set = set, get = get, 
         setinvMat = setinvMat, 
         getinvMat = getinvMat)
}


## Short description of the cacheSolve function:
## The function calculates the inverse of the special "matrix" created with the
## makeCacheMatrix function. It first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache, using the 
## getinvMat function, and skips the computation. Otherwise, it calculates the 
## inverse of the matrix ('data') using the solve function and sets the value 
## of the inverse in the cache via the setinvMat function.

cacheSolve <- function(x, ...) {
    invMat <- x$getinvMat()
    if(!is.null(invMat)) {
        message("getting the cached inverse matrix")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data)   ## assuming that 'data' is a invertible matrix
    x$setinvMat(invMat)
    invMat
}
