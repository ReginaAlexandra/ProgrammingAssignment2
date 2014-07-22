## These functions allow to cache the inverse matrix of an invertible 
## (square) matrix, which might be needed again later in a calculation. 
## Caching helps to avoid repeating the same time-consuming calculations. 

## The makeCacheMatrix function creates a special matrix-like object that
## can cache its inverse.
## set(y) sets the matrix y
## get() gets the set matrix
## setinv(y*) sets the inverse matrix y*
## getinv() gets the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function checks, whether the inverse of the object x 
## returned by makeCacheMatrix has already been calculated and cached.If so, 
## it returns the cached inverse matrix inv. Otherwise it calculates the 
## inverse matrix inv of x and returns it to be cached by makeCacheMatrix.

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
       if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
       }
       my_matrix <- x$get()
       inv <- solve(my_matrix, ...)
       x$setinv(inv)
       inv   
}

