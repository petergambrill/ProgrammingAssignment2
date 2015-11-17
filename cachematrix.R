## The two functions below allow us to create a matrix with the property
## that its inverse can be computed once and then cached.

## makeCacheMatrix ###
## This function creates a special "matrix" object that can cache its inverse.
## It takes one argument, a matrix, sets object 'i' equal to NULL and also 
## stores a list of 3 functions.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  ##will return the argument entered into the makeCacheMatrix
  getmatrix <- function() x
  
  ##will set object 'i' equal to the argument entered
  setinverse <- function(inverse) i <<- inverse
  
  ##will return the object 'i'
  getinverse <- function() i

  
  list (getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
 
}

## cacheSolve ##
## This function will output the inverse of a matrix stored using the
## 'makeCacheMatrix' function. If the inverse has already been calculated  
## it will just return the cached value from the makeCacheMatrix function, 
## (object 'i').

cacheSolve <- function(x, ...) {
  
  ## set object 'z' equal to the object 'i' from the makeCacheMatrix
  ## function. If the inverse hasn't already been calculated it will just 
  ## be NULL.
  z <- x$getinverse()
  
  ##if object 'z' is not NULL then return 'z'
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  } 
  ## Otherwie set the object 'matrix' equal to the argument entered into
  ## makeCacheMatrix function. Then set object 'z' equal to the inverse
  ## of that argument. Finally set the object 'i in the makeCacheMatrix 
  ## equal to the inverse just calculated ('z') and then return that inverse
  else {
  matrix <- x$getmatrix()
  z <- solve(matrix)
  x$setinverse(z)
  return(z)
  }
  
}
