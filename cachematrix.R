## CacheMatrix - These functions are here to help matrix inversion caching.
## Matrix inversion is a costly process. Thus, storing the result of a matrix
## inversion is a good idea


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Input: x, an inversible matrix.
## Return: getters and setters of the data and the cached inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## Making a new empty cache for the matrix inverse.
    m <- NULL
    
    ## setter for the data (non inverted matrix).
    ## when used it reset the cache to NULL (in case the data changed).
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## getter for the data
    get <- function() x
    
    ## setter for the matrix inverse.
    setinv <- function(invMatrix) m <<- invMatrix
    ## getter for the matrix inverse.
    getinv <- function() m
    
    ## We return the list of functions to manipulate the cached data.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a helper that deal with cacheMatrix to compute matrix inverse
## only if needed. If no matrix inverse is in the cache it will compute it.
## Input(s): 'x', a cacheMatrix object (returned by makeCacheMatrix).
##           Any additional argument to passe to the function 'solve()'.
## Return: The matrix inverse of 'x', from the cache if available, or computed
##         with the arguments provided.

cacheSolve <- function(x, ...) {
    ## Checking the cache to see if the matrix inverse is already stored.
    m <- x$getinv()
    if(!is.null(m)) {
        ## Returning the data from the cache with message
        message("getting cached data")
        return(m)
    }
    
    ## No data in cache, computing the matrix inverse
    
    ## Getting the matrix to be inversed
    data <- x$get()
    ## Computing the inverse matrix of 'x'
    m <- solve(data, ...)
    ## Storing the result matrix inverse in the cache
    x$setinv(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
