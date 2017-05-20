## Peer-graded Assignment: 
## Programming Assignment 2: Lexical Scoping

## This function creates a matrix object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{ 
  inverse <- NULL
  setMatrix <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function()
  {
    x
  }
  setMatrixInverse <- function(matrixName)
  {
    inverse <<- matrixName
  }
  getMatrixInverse <- function() 
  {
    inverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## cacheSolve() computes the inverse of the 
## matix returned by the makeCacheMatrix(). If the inverse
## has already been calculated then the cacheSolve() will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  inverse <- x$getMatrixInverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$getMatrix()
  inverse <- solve(data, ...) #inverse of matrix is computed here
  x$setMatrixInverse(inverse)
  inverse
}