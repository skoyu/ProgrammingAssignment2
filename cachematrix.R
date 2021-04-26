### This code defines a pair of functions that forms a special matrix object and cache its inverse.

### The first function "makeCacheMatrix" creates a special matrix object and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialization
  inv_mat <- NULL
  
  ### Set the matrix here in the environment created by the function.
  set <- function(m) {
    x <<- m
    inv_mat <<- NULL
  }
  
  ## Get the matrix here.
  get <- function() x
  
  ## Set the inverse of the matrix.
  setInverse <- function(inverse) inv_mat <<- inverse
  
  ## Get the inverse of the matrix.
  getInverse <- function() inv_mat
  
  ## Return a list of the environment created by the function. 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


### The second function "cacheSolve" computes the inverse of the matrix formed by the above function "makeCacheMatrix".
cacheSolve <- function(x, ...) {
  
  ## Extract the inverse of matrix 'x'
  inv_mat <- x$getInverse()
  
  ## Get the inverse matrix from the cache, if the inverse matrix has been calculated.
  if (!is.null(inv_mat)) {
    message("Getting Cached Data")
    return(inv_mat)
  }
  
  ## If the inverse matrix has not been evaluated, compute using the set_inverse function.  
  ## Get the matrix from the object defined in the first function.
  m <- x$get()
  
  ## Calculate the inverse using 'solve' function.
  inv_mat <- solve(m, ...)
  
  ## Set the inverse to the object.
  x$setInverse(inv_mat)
  
  ## Return the inverse matrix.
  inv_mat
}
