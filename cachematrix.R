## Contains a pair of functions to cache an inverse matrix. The value
## from makeCacheMatrix should be the first parameter to cacheSolve.

## Creates a matrix structure whose inverse matrix can be cached.
## The resulting list has the following structure:
## - set: sets the original matrix to be solved.
## - get: gets the original matrix.
## - setinverse: sets the inverse matrix of the original matrix.
## - getinverse: gets the inverse matrix of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
        solved <- NULL
        set <- function(y) {
                x <<- y
                solved <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) solved <<- inverse
        getinverse <- function() solved
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of a matrix if it has been solved. Otherwise, it
## will solve the inverse matrix and cache it for future use.

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
