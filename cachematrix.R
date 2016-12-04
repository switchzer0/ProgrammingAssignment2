## The two functions in this pair work together to format a matrix to cache its inversion, saving
## large amounts of computation time with large matrices.


## This first function below formats a matrix so its inversion can be cached, outputting a list
## of getters and setters to be called by the other function in the pair (cacheSolve).
## **input matrix must be invertible i.e. a square non-singular matrix**

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this second function below inverts matrices that have been formatted per the above makeCacheMatrix
## function. It seeks the cached value of the matrix inversion if it has already been calcuated
## **input must be a makeCacheMatrix-formatted list; a matrix entered directly will not compute properly**

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
