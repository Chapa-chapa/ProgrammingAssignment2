## This pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMat <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(setMat = setMat,
         getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$getMat()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
