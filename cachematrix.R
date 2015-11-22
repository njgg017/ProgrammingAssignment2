## This function to cache the inverse matrix from a original matrix, if that original matrix has
## an inverse.

## In this function, the original matrix and the inverse matrix are stored, and 
## then these matrices are solved in the other function. Here are prepared and stored
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL 
  set <- function(set_matix) {
    x <<- set_matix
    inverse_matrix <<- NULL 
  }
  get <- function() x
  setInv <- function(inverse_func) inverse_matrix <<- inverse_func 
  getInv <- function() inverse_matrix
  
  list(set = set, get = get, setInv = setInv,getInv = getInv)
  
  
}

## In this function, the inverse matrix is calculated with solve function. If original matrix was stored, 
## the inverse matrix that was stored is returned, so don't to recalculate the inverse matrix
cacheSolve <- function(x, ...) {
  to_solve <- x$getInv()
  if(!is.null(to_solve)) { 
    message("getting cached data")
    return(to_solve) 
  }
  output <- x$get()
  to_solve <- solve(output)
  x$setInv(to_solve)
  to_solve
}