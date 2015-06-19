## Making two functions to calculate and store the inverse of a matrix. 

## Makes a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get <- function() x 
    setinverse <- function(solve) i <<- solve 
    getinverse <- function() i 
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" stored in makeCacheMatrix. If 
## matrix has already been solved, it is first retrieved before calculating anew.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) { 
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
