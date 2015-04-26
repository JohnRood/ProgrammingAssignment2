## Put comments here that give an overall description of what your
## functions do

## As per the requrements of the assignment:
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

## It is necessary to use as.matrix on the returned inverse "matrix".

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## m is for (inverse) matrix
## Defines four functions for doing the minimal set of basic operations on our matrix x

## Initialize
        m <- NULL
        
## set (or reset) the m matrix x to be y.  Note this makes any previous m (presumably) useless.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## obvious
        get <- function() x
## set the computed inverse in "cache" (if necessary)
        setinv <- function(solve) m <<- solve
## get the inverse
getinv <- function() m
## Technically necessary to define the (sub)functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x) {
## retrieve precomputed inverse (if possible)
        m <- x$getinv()
## Was there anything to retrive?  If so, return it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## Otherwise compute it, set it in our "cache matrix", and return it from this function
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}