## makeCacheMatrix takes a matrix as an argument and creates a special list containing functions to
##        set the value of the matrix
##        get the value of the matrix
##        set the inverse of the matrix
##        get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverseMat <- function(solve) m <<- solve
     getInverseMat <- function() m
     list(set = set, get = get,
          setInverseMat = setInverseMat,
          getInverseMat = getInverseMat)
}


## cacheSolve returns the inverse of the matrix created by makeCacheMatrix
##        It first checks to see if the matrix has already been inverted
##        If it has it returns the result from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
     
     m <- x$getInverseMat()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverseMat(m)
     m
     
}
