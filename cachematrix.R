## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix has a matrix for parameter
# Setorigin and Getorigin the value of the matrix
# Setinverse and Getinverse the inverse matrix
# This way the matrix cached itself
# We use the <<- to assign a value to an object in an environment different from the current
makeCacheMatrix <- function(x = matrix()) {
  m_inv = NULL
  setorigin = function(y) {
    x <<- y
    m_inv <<- NULL
  }
  getorigin = function() x
  setinverse = function(inverse) m_inv <<- inverse 
  getinverse = function() m_inv
  list(setorigin=setorigin, getorigin=getorigin, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

## cachesolve take the reult of the previous function 'makeCacheMatrix' as parameter
# First check if the inverse matrix is empty or not
# -> If the inverse matrix resulted from makeCacheMatrix((matrix) has been calculated
# then it caces the object and send a message
# -> If the inverse matrix resulted from makeCacheMatrix((matrix) is empty
# # then it get the original matrix and and gets the inverse through solve function
cacheSolve <- function(x, ...) {
  inverse = x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix.data = x$getorigin()
  inverse = solve(matrix.data, ...)
  x$setinverse(inverse)
  ## Return a matrix that is the inverse of 'x'
  return(inverse)
}