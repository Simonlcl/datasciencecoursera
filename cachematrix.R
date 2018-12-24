## It's an R function that is able to cache potentially time-consuming computations.
## by caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set&get the value of the matrix
## set&get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmsolve(m)
        m
        
     
}
