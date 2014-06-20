## David Ellis (ellisda@gmail.com)
## Coursera "R Programming" - Programming Assignment 2
## https://github.com/ellisda/ProgrammingAssignment2
## -------------------------------------------------
## This assignment contains functions to create a special matrix object
## that stores a cached Inverse of itself, allowing for fast retrieval
## without computation each time it is accessed.



## This function creates a special "matrix" object that can cache its inverse
##
## makeCacheMatrix returns an object as a list of functions: get, set,
## getinverse, and setinverse. Tthis object will be used by the
## cachSolve function to check cached solution

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
## Check for a cached value for the inverse of matrix and return it if found
## If no cached value is found, compute the inverse and store it in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) #compute inverse of sqaure invertable matrix x
    x$setinverse(m)
    m
}


