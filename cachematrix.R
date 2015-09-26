## caching the inversion of a matrix
## make these sources following "Caching the Mean of a Vector"

## create a special matrix object that can cache its inverse.
## 1. get the matrix
## 2. set the matrix
## 3. get the inverse of matrix
## 4. set the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list (set = set, get = get, 
          setsolve = setsolve,
          getsolve = getsolve)
}


## computes the inverse of matrix using cache 
## on speical matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}