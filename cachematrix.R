## cachematrix.R file contains function for  
## caching the inverse of a matrix in order to prevent 
## costly computations.



## makeCacheMatrix() function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmatrix <- function(matrix) m <<- matrix
   getmatrix <- function() m
   list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## cacheSolve() computes the inverse of the special "matrix" returned
## by the function makeCacheMatrix()
## If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
   m <- x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   # solve returns the inverse of the matrix,
   # assuming the matrix is always invertible. 
   m <- solve(data, ...)  
   x$setmatrix(m)
   m
   
}
