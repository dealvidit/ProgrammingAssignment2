##Following are 2 functions which basically create a special matrix (which really is a list)
##which can cache the inverse it calculated thus saving time by returning the cached value
##instead of calculating the entire problem again.



## This fuction creates that "special" matrix with the caching ability.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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

##This function calculates and returns the inverse of the above function's
## created special matrix, but prior to that, checks if the inverse is
##already been calculated, and if so, returns that "cached" answer.
cacheSolve <- function(x, ...) {
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
