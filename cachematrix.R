## makeCacheMatrix creates a special matrix object,  
## and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated,  
## it will instead find it in the cache and return it, and not calculate it again.
 
makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## get the value of the matrix
    get <- function() x

    ## set the inverse of the matrix
    setInverse <- function(inverse) i <<-inverse
    getInverse <- function() i

    ## get the inverse of the matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
 

## The function cacheSolve returns the inverse of a matrix A created with the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    ## get the inverse of the matrix.
    i <- x$getInverse()

    ## check if there is the matrix 
    if (!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    } else {
    ## get the inverse of the matrix
        i <- solve(x$get())
        x$setInverse(i)
        return(i)
    }
}
