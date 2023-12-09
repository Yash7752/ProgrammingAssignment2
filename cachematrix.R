## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # To store the cached inverse
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y  # Set the matrix
        inv <<- NULL  # Clear the cached inverse
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to cache the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Function to get the cached inverse
    getinverse <- function() inv
    
    # Return a list of functions for setting, getting, caching, and getting the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Compute the inverse of the special "matrix" and cache it
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Try to get cached inverse
    
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    mat <- x$get()  # Get the matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setinverse(inv)  # Cache the inverse
    inv  # Return the computed inverse
}
