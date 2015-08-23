
# makeCacheMatrix creates a special "Matrix" object that can cache its inverse
# This conatains following functions: 
# 1. set            set the value of a matrix
# 2. get            get the value of a matrix
# 3. setInverse     get the cahced value (inverse of the matrix)
# 4. getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
  
  # initialize the value of cache to NULL
  cache <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # Get the value of matrix
  get <- function() {
    x
  }
  
  # cache the given argument 
  setInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # Each element of the list is a function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # get the cached value
  m <- x$getInverse()
  # if a cached value exists return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  
  # return the inverse
  m
}
