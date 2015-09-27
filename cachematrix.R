## The pair of functions below work together to cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get = function() x
    setinv = function(inverse) inv <<- inverse
    getinv = function() inv
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## This function computes the inverse of the matrix object returned by the 
## makeCacheMatrix function. If the inverse has already been calculated, then 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    matrix = x$get()
    inv = solve(matrix, ...)
    
    x$setinv(inv)
    inv
}
