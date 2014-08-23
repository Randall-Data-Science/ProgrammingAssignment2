## Put comments here that give an overall description of what your
## functions do

## This `makeCacheMatrix` function, when called with a matrix, creates a list which
## contains that matrix, and all an empty container for its inverse termed `s`.
## The matrix can be retrieved with `<matrixname>$get()`.

makeCacheMatrix <- function(x = numeric()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns the inverse of the matrix stored in the makeCacheMatrix
## list/object. It does this in one of two ways. If the inverse has been calculated
## before, then the inverse is waiting in `s`. If the inverse has NOT been calulated
## before, then `s` is NULL, and this function first calculates the inverse, then 
## stores it in `s`, and then it returns the inverse. The next time the inverse is
## requested, it will be retrieved from the cache.

cacheInverse <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}

## For a longer explanation, as well as documented timings, see: 
## https://github.com/Randall-dSci/ProgrammingAssignment2/blob/master/Explanation.html
## found in this repository

## Viewable here:
### https://rawgit.com/Randall-dSci/ProgrammingAssignment2/master/Explanation.html
