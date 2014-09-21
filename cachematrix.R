## Group of functions that cache the inverse of a matrix


## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse
  i <- NULL
  
  ## Create function to set the matrix
  fset <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Create function to get the matrix
  fget <- function() {
    ## Return the matrix
    x
  }

  ##Create function to set inverse of the matrix
  fsetInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##Create function to get inverse of the matrix
  fgetInverse <- function() {
    ##Return the inverse
    i
  }
  ## Return a list of the functions
  list(fset = fset, fget = fget, fsetInverse = fsetInverse, fgetInverse = fgetInverse)
  
}


## Calculate the inverse of the matrix returned by makeCacheMatrix function.
## Assuming the inverse has already been calculated, 
##the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$fgetInverse()
  
  ## if it is already set, then return the inverse
  if(!is.null(mat)) {
    message("retrieving the cache data")
    return(mat)
  }
  
  ## Retrieve the matrix from the object
  data <- x$fget()
  
  ##Compute the inverse using matrix multiplication
  mat <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$fsetInverse(mat)
  
  ## Return the matrix
  mat
  
}
