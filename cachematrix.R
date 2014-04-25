##
## This file implements a special matrix that can cache the 
## resuls of computing its inverse
##

## 
## Creates a cache matrix capable of caching
## its inverse. 
## Arguments:
## x - matrix to wrap in the 'special' matrix
## Returns:
## The cache matrix which is a list with following functions:
## set(m): sets the value of the matrix
## get(m): gets the value of the matrix
## setInverse(i): sets the value of the inverted matrix
## getInverse(): gets the value of the invested matrix or NULL if it has been set
## 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    list(
        set = function(m) { 
            x<<- m
            inverse <<- NULL
        },
        get = function() { x },
        setInverse = function(i) {inverse <<- i},
        getInverse = function() { inverse }
    )
}


##
## Computes and caches inverse of a cache matrix created with makeCacheMarix
## Arguments:
## x - cache matrix
## Returns:
## a matrix which is the inverse of the matrix wrapped by x

cacheSolve <- function(x, ...) {
    if (is.null(x$getInverse())) {
        print("Calculating inverse")
        x$setInverse(solve(x$get()))
    }
    x$getInverse()
}
