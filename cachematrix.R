##This file defines functions that return  cached inverse of a matrix to avoid recomputation
##Sample usage: 
##a = matrix(c(1,1,1,1), 2,2)
##x1 = makeCacheMatrix(a)
##cacheSolve(x1)
##   [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0
##cacheSolve(x1)
##getting cached data
##     [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0

## makeCacheMatrix is  a special matrix that caches the inverse of a invertible square matrix x,
## It returns a list of accessor functions to retrieve the matrix and its inverse. 
##The matrix input must be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Finds the inverse of the special matrix defined using makeCacheMatrix,
## Stores the computed inverse back in makeCacheMatrix for to avoid recomputation the next time
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

