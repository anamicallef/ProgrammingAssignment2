## This function is able to calculate the inverse of a matrix and
## cache its value.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse
## This function is a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(A = matrix()) {
        M <- NULL
        set <- function(B) {
                A <<- B
                M <<- NULL
        }
        get <- function() A
        setinverse <- function(solve) M <<- solve
        getinverse <- function() M
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache
## and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(A, ...) {
        M <- A$getinverse()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- A$get()
        M <- solve(data, ...)
        A$setinverse(M)
        M
}
