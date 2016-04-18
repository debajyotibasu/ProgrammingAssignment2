## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
