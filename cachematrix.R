## Below are two functions that are used to create a special object that stores a  vector and caches its inverse.

## Make get/set functions for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL           #will store inverse later when makeCacheMatrix is used in cacheSolve
    set <- function(y) {
        x <<- y         #reset value of x and i in all of makeCacheMatrix 
        i <<- NULL
    }
    get <- function() x  
    setinv <- function(inv) i <<- inv   
    getinv <- function() i             
    list(set = set, get = get, setinv = setinv, getinv = getinv) #store all four functions within makeCacheMatrix
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
