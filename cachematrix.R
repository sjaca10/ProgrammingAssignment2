## Here there are two functions that are used to create a special object that
## stores a numeric matrix and cache's its inverse matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse
## through four anonimous functions:
## set <- store the value of the matrix
## get <- return the value of the matrix
## setsolve <- store the value of the inverse matrix
## getsolve <- return the value of the inverse matrix
## @param x <- a variable of matrix type
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve computes the inverts of the special matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated, then this
## function should retrieve the result from the cache, in other case it should
## calculate, cached and return the result
## @param x <- a variable of special matrix type
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){
        message("Getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
}