## The makeCacheMatrix and cacheSolve functions allow the caching of the value for the inverse of a matrix, so that the
## inverse of the matrix doesn't have to be recalculated every time it is needed.

## makeCacheMatrix makes a list of functions to 1. set the value of the matrix 2. get the value of the matrix 3. set
## the value of the inverse of the matrix 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
        # Anytime the contents of the matrix changes, the value of the inverse (m) is reset to null
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks whether the inverse of a matrix has already been calculated. If it has, cacheSolve returns the already
## calculate value. If the inverse has not already been calculated, then the function calculates the inverse and returns the new
## value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
