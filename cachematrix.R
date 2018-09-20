## makeCacheMatrix: Build a set of functions and return them in a 
## list to the parent environment.
makeCacheMatrix <- function(x = matrix()) {
  
  ### initialize the inverse of the matrix to NULL.
  inv_mtx <- NULL
  
  ## Following the example of makeVector(), define the get and set functions.
  set <- function(arg1) {
    x <<- arg1
    inv_mtx <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv_mtx <<- solve
  getsolve <- function() inv_mtx
  
  ## Create a list whose elements are the methods defined above.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
  

## Caclulate and populate, or retrieve, the inverse matrix
## from an object of type makeCacheMatrix.        
cacheSolve <- function(x, ...) {
  
  ## Check to see if a cached inverse of the matrix is available.
  ## If it is, return it to the parent environment.
  inv_mtx <- x$getsolve()
  if(!is.null(inv_mtx)) {
    message("Returning the cached inverse")
    return(inv_mtx)
  }
  
  ## If the inverse is not available, get the matrix from the input object,
  ## run solve to get the inverse of the matrix and return it.
  data <- x$get()
  inv_mtx <- solve(data, ...)
  x$setsolve(inv_mtx)
  inv_mtx
}
