## The script contains functions which would cache the inverse of a matrix
## So the costly computation occurs only once

## Function returns a list of supporting functions which would cache
## matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invMat <- NULL
  
  #Function to set a new Matrix
  set <- function(newMat){
    # Assign the matrix value of y to x
    x <<- y
    
    #Assign the value of null to invMat since old value 
    #would be obsolete
    invMat <<- NULL
  }
  
  # Returns the original Matrix x
  get <- function() x
  
  # Sets the inverse of the matrix x
  setInverse <- function(inverseMatrix) invMat <<- inverseMatrix
  
  # Function returns the inverse of the matrix x
  getInverse <- function() invMat
  
  # Prepare a list of the functions and return the same
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse matrix of the matrix x

cacheSolve <- function(x, ...) {
  # Get the stored inverse matrix
  invMat <- x$getInverse()
  # Check whether the inverse is already computed
  if(!is.null(invMat)) {
    message("getting cached data")    
  }
  else{
    # Calculate the matrix inverse and store it in x
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInverse(invMat)  
  }  
  ## Returns a matrix that is the inverse of 'x'
  invMat
}
