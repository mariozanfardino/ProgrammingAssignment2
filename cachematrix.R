## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  ## This function get in input a matrix and return a list containing functions to
  ## set the matrix, get the matrix,set the inverse and get the inverse.
  ## The created list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  
  set = function(y) { 
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  
  setinverse = function(inverse) inv <<- inverse 
  
  getinverse = function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function

  ## The output of makeCacheMatrix() is the input of cacheSolve() function, which
  ## return the inverse of the original matrix
    
cacheSolve <- function(x, ...) {

  inv = x$getinverse()
  
  if (!is.null(inv)){
    message("using cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinverse(inv)
  
  return(inv)
}
