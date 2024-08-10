## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse. It returns a list of functions that allow setting and getting 
## the matrix value, as well as setting and getting the cached inverse.

##The cacheSolve function is used to compute and cache the inverse of a matrix.
##It checks if the inverse is already available in the cache to avoid redundant 
##computations.

## Write a short comment describing this function
##This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the value of the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the value of the inverse
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix. If the inverse has already been 
##calculated and the matrix has not changed, then cacheSolve 
##retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the calculated inverse
  x$setInverse(inv)
  
  # Return the inverse matrix
  inv
}
