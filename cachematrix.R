## The following functions demonstrate how it is possible to cahce the results 
## of an expensive calculation (like matrix inversion) for future use using
## using the lexical scoping feature of R.
## The code assumes that the matrix is invertible

## The following function creates a cached "matrix" object that really returns
## a list of functions that allows the matrix and its cached inverse
## to be accessed

makeCacheMatrix <- function(x = matrix()) {
    ## Variable for storing the inverse in the outer environment
    x_inverse <- NULL
    
    ## Set the cached matrix to a new matrix
    set <- function(y) {
        ## Only update the stored values if the matrix has changed
        if (!isTRUE(all.equal(x, y))) {
            ## Use the <<- operator to store in the outer environment
            x <<- y
            x_inverse <<- NULL
        }
    }
    
    ## Return the cached matrix
    get <- function() {
        x
    }
    
    ## Set the cached inverse matrix
    setinverse <- function(inverse) {
        x_inverse <<- inverse
    }
    
    ## Return the cached inverse matrix. May be NULL if it has not yet been set
    getinverse <- function() {
        x_inverse
    }
    
    ## Return a list of functions that can be used to access the cached matrix
    ## and its inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a matrix created with the 
## makeCacheMatrix function. If the matrix has already been calculated, it 
## returns the cached value, otherwise it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Getting cached inverse")
        return(inverse)
    }
    cache_matrix <- x$get()
    inverse <- solve(cache_matrix, ...)
    x$setinverse(inverse)
    inverse
}
