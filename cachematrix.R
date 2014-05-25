## This function stores input matrix in the cache and creates structure with 
## several functions in it:
## get()        - function to get stored matrix
## getinverse() - function to get inversed matrix from the cache
## set()        - function to set new values to the stored matrix,
##                deletes all cache with inversed matrix
## setinverse() - function to store inversed matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    #function to change matrix stored in the structure.
    #Deletes inverted matrix cache, also.
    set <- function(newMatrix) {
        x <<- newMatrix
        invX <<- NULL
    }

    #function to get original matrix from the structure
    get <- function() x

    #function to save inversed matrix in cache
    setinverse <- function(inverse) invX <<- inverse

    #function to get inversed matrix from the cache
    getinverse <- function() invX    

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function gets the structure, created by makeCacheMatrix function,
## checkes, whether cache with inversed matrix inside this structure is empty
##   * retrieves inversed matrix from the cache if cache is not empty,
##   * calculates and store inversed matrix in cache if cache is empty

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check the cache first
    invX <- x$getinverse()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    
    #No matrix in cache, calculate it instead
    data <- x$get()
    invX <- solve(data, ...)
    x$setinverse(invX)
    invX
}
