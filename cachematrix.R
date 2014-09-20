## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        m1 <- matrix(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse), 2, 2)
	  names(m1) <- c("set", "get", "setinverse", "getinverse")
	  m1
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed, (checked using the identical function..)),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        data <- x$get()
        if(!is.null(i) && identical(x, data)) {
                message("getting cached data")
                return(i)
        }
        i <- solve(data)
        x$setinverse(i)
        i
}