## The first function makeCacheMatrix() creates a (square) matrix. This function
## is able to cache the inverse of the matrix, which is either computed from the 
## input given in makeCacheMatrix, or directly from the "set" function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve uses the solve() function to compute the inverse of
## the matrix that is produced by the function above (makeCacheMatrix). The if-
## statement checks if the inverse has or has not been set by the set() function 
## in makeCacheMatrix(). If it has not been set, the cacheSolve function calculates
## the inverse of the matrix, if it has been set previously using in the
## makeCacheMatrix() function, cacheSolve will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
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
