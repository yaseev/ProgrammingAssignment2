## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following pair of functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object,
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # s will store the cached inverse matrix
        s <- NULL
        
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # Getter for the matrix
        get <- function() {
                x
        }
        
        # Setter for the inverse
        setSolve <- function(solve) {
                s <<- solve
        }
        
        # Getter for the inverse
        getSolve <- function() {
                s
        }
        
        # Return the matrix with the newly defined functions
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        
        # If the inverse is already calculated, return it
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # The inverse is not yet calculated, so it will be calculated
        data <- x$get()
        s <- solve(data, ...)
        
        # Cache the inverse
        x$setSolve(s)
        
        # Return the inverse
        s
}


