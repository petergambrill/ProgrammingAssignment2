## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i

  
  list (getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$getmatrix()
  i <- solve(matrix)
  x$setinverse(i)
  return(i)
}
