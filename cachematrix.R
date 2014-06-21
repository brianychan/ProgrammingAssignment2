## These functions combine to invert a matrix, while utilizing caching for efficiency
## We assume a square invertible matrix                   


## makeCacheMatrix creates a list object of functions that facilitate matrix caching

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                       ##declare cache object
        set <- function(y) {
                x <<- y                                 ##function: set matrix and reset cache
                i <<- NULL
        }
        get <- function() x                             ##function: retrieve value for x
        setinverse <- function(inverse) i <<- inverse   ##function: set inverse as global value for i
        getinverse <- function() i                      ##function: retrieve value for i
        
        list(set = set, get = get,                      ##return list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix
## The function checks to see if the inverse of the matrix is cached:
## If the inverse is in the cache, it is retrieved and returned. 
## If the inverse is not cached, the inverse is solved for, returned, and cached.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                            ##testing for a cached object
        if(!is.null(i)) {                              
                message("getting cached data")         ##in the event of a cached object:
                return(i)                              ##show message and return cache
        }
        
        data <- x$get()                                ##in the event of no cached object:
        i <- solve(data)                               ##solve, cache, and return result
        x$setinverse(i)
        i
}
