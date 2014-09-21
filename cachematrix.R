## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	          i <- NULL
	  identical <- FALSE
        set <- function(y) {
	    identical <<- identical(x, y)
	            x <<- y
        	    i <<- NULL	
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
	setidentical <- function(ident) identical <<- ident
	getidentical <- function() identical
        m1 <- matrix(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, setidentical = setidentical, getidentical = getidentical), 3, 2)
	names(m1) <- c("set", "get", "setinverse", "getinverse", "setidentical", "getidentical")
	m1
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## checked using the value of identical stored.
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
	identical <- x$getidentical()
        if(!is.null(i) && identical) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
	x$setidentical(TRUE)
        i
}
