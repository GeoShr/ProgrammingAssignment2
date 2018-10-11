makeCacheMatrix <- function(x = matrix()) {
  ### Modified 10/11/18
  ### initialize the inverse of the matrix to NULL.
  inv_mtx <- NULL
  
  ### Following the example of makeVector(), define the get and set methods.
  set <- function(arg1) {
    x <<- arg1
    inv_mtx <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_mtx <<- solve
  getinv <- function() inv_mtx
  
  ### Create a list whose elements are the methods defined above.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
  

          
cacheSolve <- function(x, ...) {
  
  ### Check to see if a cached inverse of the matrix is available.
  ### If it is, return it to the parent environment.
  inv_mtx <- x$getinv()
  if(!is.null(inv_mtx)) {
    message("Returning the cached inverse")
    return(inv_mtx)
  }
  
  ### If the inverse is not available, get the matrix from the input object,
  ### run solve to get the inverse of the matrix and return it.
  data <- x$get()
  inv_mtx <- solve(data, ...)
  x$setinv(inv_mtx)
  inv_mtx
}

# To run 
# tst_mtx<-stats::rnorm(16)
# dim(tst_mtx)<-c(4,4)
# myMatrix<-makeCacheMatrix(tst_mtx)
# cacheSolve(myMatrix)
