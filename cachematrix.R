#       Example usage:
#       > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
#       > cx <- makeCacheMatrix(x)                  // Create our special matrix
#       > cx$get()                                  // Return the matrix
#       > cacheSolve(cx)                            // Return the inverse
#       > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse


# makeCacheMatrix: return a list of functions to:
# set           - Set the value of the matrix
# get           - Get the value of the matrix
# setinverse    - Set the value of the inverse
# getinverse    - Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ##    set the value of the matrix
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##    get the value of the matrix
  get <- function() x
  
  ##    set the inverse of the matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ##    get the inverse of the matrix
  list(set = set, get = get,
       sextinverse = setinverse,
       getinverse = getinverse)
}
 
 
# cacheSolve:     Compute the inverse of the matrix. If the inverse is already
#                 calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  i <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  i <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(i)
  i
}
