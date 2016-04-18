## makeCacheMatrix returns an set of functions e.g get, set, getinverse and setinverse on a matrix



makeCacheMatrix <- function(x = matrix()) {
      
    Y <- NULL
    set <- function(y) {
      x <<- y
      Y <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) Y <<- inverse
    getinverse <- function() Y
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve function returns the inverse of a matrix from cache if exists else calculates the inverse,
## stores in the cache and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Y <- x$getinverse()
        
        if(!is.null(Y)){
          message("Getting inverse value from cache...")
          return(Y)
        }
        
        data <- x$get()
        Y <- solve(data)
        x$setinverse(Y)
        
        Y
}
