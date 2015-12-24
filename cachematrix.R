## The functions makeCacheMatrix and cacheSolve help in speeding up matrix inversion by 
# caching a computed matrix inverse


## makeCacheMatrix creates a special matric object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- as.matrix(list())
    # Set the matrix
    setMat <- function(y) {
        x <<- y
        inv <<- as.matrix(list())
    }
    # Get the matrix
    getMat <- function() x
    # Set the matrix inverse
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(setMat = setMat, getMat = getMat, 
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve computes the inverse of the special matrix obtained from makeCacheMatrix. 
# However, if the inverse has already been calculated, and has not changed, the function 
# retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    # Check if the inverse has already been calculated, and is the same as before
    if(!all(is.na(inv))) {
        message("Getting cached matrix inverse")
        return(inv)
    }
    # Otherwise, compute the inverse
    matData <- x$getMat()
    inv <- solve(matData, ...)
    x$setInv(inv)
    inv
}





