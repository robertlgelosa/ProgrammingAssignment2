## 
## These functions work together to create a cache of an inverse
## matrix. 

## This function creates a list of 4 functions.
## The argument of this function
## is the matrix you want inverted. 
## 
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

## cacheSolve finds the inverse of the matrix
## in argument for makeCacheMatrix and caches it.
## Set makeCacheMatrix to a variable.
## Pass this variable into cacheSolve as the argument.
## cacheSolve will use solve and return the inverse.
## If you already ran cacheSolve with this same argument
## it will not use solve, but simply return the 
## cached inverse. 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        our_matrix <- x$get()
        m <- solve(our_matrix, ...)
        x$setinverse(m)
        m
}