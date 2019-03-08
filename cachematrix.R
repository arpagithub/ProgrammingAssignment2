## The makeCacheMatrix and cacheSolve matrix function creates a special object that stores a matrix and caches its inverse. 

## The function makeCacheMatrix creates a special "matrix" and contains a list containing set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will compute the inverse of the special matrix.Also, if inverse is already being calculated then it will pick up from cached data

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
