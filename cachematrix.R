## This file contains makeCacheMatrix and cacheSolve functions provide methods
## to create an inverted matrix from a provided matrix. And, when available,
## use a cached version of the matrix instead of recomputing each time.
#' @author https://github.com/jeff-phil


#' Function that provides a list getters and setters for the caching
#'   of a matrix, and the inversion of that matrix.
#' 
#' makeCacheMatrix$get() will return the current non-inverted matrix 
#'   if exists
#' makeCacheMatrix$set() will set the current non-inverted matrix, 
#'   and clear the cached value.
#' makeCacheMatrix$getinverse() will return the cached inverted matrix, 
#'   if exists
#' makeCacheMatrix$getinverse() will cache the inverted matrix
#' 
#' @param x a matrix (that assumes can be inverted).
#' @return a list of functions for getting and setting data
#' @examples
#' ## This example from solve() help to create a Hilbert matrix
#' hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#' h8 <- hilbert(8)
#' m <- makeCacheMatrix(hilbert(8))
#' 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#' Function that inverts the special matrix from list returned
#'   by makeCacheMatrix function
#' 
#' @param x a list created by the makeCacheMatrix function
#' @param ... further arguments passed to or from other methods to be
#'   added to solve() function
#' @return a matrix that is the inverse of matrix provided by x
#' @examples
#' ## This example from solve() help to create a Hilbert matrix
#' hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#' m <- makeCacheMatrix(hilbert(8))
#' cacheSolve(m)
#' 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(is.null(m)) {
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
        } else {
                message("getting cached inverted matrix")
        }
        m
}
