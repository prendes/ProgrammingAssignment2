# Matrix inversion, a costly computation, can benefit from caching the 
# result instead of repeated computations. Herein are two functions to
# calculate and cache the inverse of a matrix.

# Alvaro Prendes, Coursera R Programming, 2017

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse property
    inv<-NULL
    
    ## Set the new matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Return the matrix value
    get <- function() x
    ## Set the inverse value
    setinverse <- function(inverse) inv <<- inverse
    ## Return the inverse value
    getinverse <- function() inv
    ## List all operations
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# caclulcation. If not, it calculates the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Validate existance of cached data
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }

    ## Get the matrix
    data <- x$get()

    ## Solve the matrix
    m <- solve(data) %*% data

    ## Get the inverse
    x$setInverse(m)

    ## Return the inverse
    m    
}
