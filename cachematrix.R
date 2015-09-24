## Programming Assignment 2
## Inverse of a Matrix

## Function makeCacheMatrix is a function that stores a list of functions
## The functions in the list are getmatrix, setmatrix, getinverse and setinverse
## Takes as argument a matrix and assign this matrix or its inverse
## If wanted, setmatrix can be used to change the matrix in the main function
## The solve function solves a equation, taking either a vector or a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve is a function that has as argument a matrix and returns the inverse of that matrix
## First the function verifies m, stored previously ith getinverse
## In case it exist in memory, it rerutns a message and the corresponding m value
## If m is null, then it will get the matrix and compute the inverse

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
