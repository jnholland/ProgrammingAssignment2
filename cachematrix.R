## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a matrix object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     set_inverse <- function(inverse) inverse <<- inverse
     get_inverse <- function() inverse
     list(set = set,
          get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)
}


## Write a short comment describing this function
##  this function calculates the inverse of the matrix created above, but if the inverse has already been created then R gets this inverse from the cache. 

cacheSolve <- function(x, ...) {
     inverse <- x$get_inverse()
     if (!is.null(inverse)) {
          message("getting cached inverse")
          return(inverse)
     }
     inv_mat <- x$get()
     inverse <- solve(inv_mat, ...)
     x$set_inverse(inverse)
     inverse
}
