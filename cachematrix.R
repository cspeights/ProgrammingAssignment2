## These functions cache the inverse of a matrix so that it does not have to repeatedly calculated

## This function creates a matrix object and a place to cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks to see if the inverse is already calculated, if not, the function calculates the inverse and returns it 

        cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
