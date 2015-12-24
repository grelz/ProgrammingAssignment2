## This function creates a special "matrix" object that can cache its inverse
## А "matrix" object is a list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the the Inverse of a Matrix
##   get the value of the the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
 
  # initialize s
  s <- NULL  
 
  # set function
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # get function
  get <- function() x
  
  # setsolve function
  setsolve <- function(solve) s <<- solve
  
  # getsolve function
  getsolve <- function() s
  
  # Return a list containing a function   
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # check cache
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # compute the inverse and save to cache
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  # Return a matrix that is the inverse of 'x'  
  s   
}
