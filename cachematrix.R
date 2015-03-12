## The following two function allow getting the inverse of a square matrix
## repeatedly, without redetermining the inverse each time. In other words the
## inverse is cached.
## function makeCacheMatrix is used to create the "matrix"
## function cacheSolve to retrieve the inverse (and cache it if not done yet)

## creates a "matrix" that can cache its inverse. In fact, a list is created
## containg functions to set the matrix, get it, set its inverse (cache) and get it
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL      ## inverse/cache is unknown initially, of course
    set <- function(y) { ## (re)set the matrix to be inverted potentially
        x <<- y          ## remember the matrix
        inverse <<- NULL ## inverse/cache not determined on setting the matrix
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv ## sets the cache/inverse
    getInverse <- function() inverse ## gets the cached inverse (NULL if not set yet)
    list( set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse )
}


## gets the inverse of a square "matrix" created using the function makeCacheMatrix,
## checking if the inverse is cached first. If no value is cached, the inverse
## is determined and the result is both set in the cache and returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse() 
    if(!is.null(inverse)) { ## inverse is cached already --> return cached value
        message("getting cached inverse")
        return(inverse)
    }
    m <- x$get() ## get the real matrix to be able to determine its inverse
    inverse <- solve(m, ...) ## determine the inverse
    x$setInverse(inverse)    ## don't forget to update the cache with the result
    inverse                  ## return the result=inverse
}
