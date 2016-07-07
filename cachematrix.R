## Assignment 2: R Programming - Tim Miller
## Create 2 functions for Caching the Inverse of a Matrix
## If the inverse has already been calculated, it should be cached.  The cache should be pulled.

## Create the special matrix object that can hold the matrix inverse as cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<-y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Compute the inverse of the special matrix returned by makeCacheMatrix.  If the inverse
## has already been calculated and the matrix has not changed, then the inverse should be
## retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
