## CACHE MATRIX INVERSES
# These functions work in tandem to cache the inverse of a matrix (eg. to save
# calculation time in a loop where the matrix is not changing in every
# iteration.)
#
#
# Usage Example:
#
# > A <- matrix(c(1,2,3, 4,0,4, 3,2,1), 3, 3)
# > CM <- makeCacheMatrix(A)
# > for(idx in 1:3) { inv <- cacheSolve(CM) }
# 
# >>> cacheSolve: Calculating inverse.
# >>> cacheSolve: Using cached inverse.
# >>> cacheSolve: Using cached inverse.



##  makeCacheMatrix - CREATE A CACHED MATRIX OBJECT ###########################
# This function creates a vector to store functions implementing a cache for a
# matrix inverse. It provides set(), get(), set_inverse(), and get_inverse()
# functions
makeCacheMatrix <- function(x = matrix()) {
  # If .matrix_inverse is NULL, the inverse is out-of-date
  matrix_inverse <- NULL
  
  # Define accessor functions
  set_fxn <- function(new_matrix) {
    x <<- new_matrix
    matrix_inverse <<- NULL # <<- assigns in makeCacheMatrix environment
  }
  #   get - Return matrix contents
  get_fxn <- function() { return( x ) }
  # set_inverse - For setting matrix inverse
  set_inverse_fxn <- function(arg) { matrix_inverse <<- arg }
  #   get_inverse - Returns the matrix inverse
  get_inverse_fxn <- function() { return( matrix_inverse ) }
  
  # Return a list with named function parameters
  return( list( set = set_fxn,
                get = get_fxn,
                set_inverse = set_inverse_fxn,
                get_inverse = get_inverse_fxn) )
}



##  cacheSolve - CALCULATE AND CACHE INVERSE OF MATRIX  #######################
# This function leverages the R "solve" function to determine the inverse of
# a matrix and then saves it to an internal cache for future access.
# Note: The two messages are purely for demonstration
cacheSolve <- function(cache_matrix, ...) {
  
  # Get stored inverse
  inverse <- cache_matrix$get_inverse()
  
  # Calculate a new inverse if the stored value is out-of-date
  if(is.null(inverse)) {
    message(">>> cacheSolve: Calculating inverse.")
    inverse <- solve( cache_matrix$get(), ... )
    cache_matrix$set_inverse(inverse)
  } else {
    message(">>> cacheSolve: Using cached inverse.")
  }
  
  return( inverse )
}
