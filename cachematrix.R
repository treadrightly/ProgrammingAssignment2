# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
# input param x: the output of makeCacheMatrix()
# input param y: the matrix whose inverse is to be computed (ideally, should be same as x$get())
# the function internally checks if x$get() is identical to y
# if not identical, then the cache is cleared and the inverse recomputed
# no error checking is done - e.g., check if x is actually a structure output from
# makeCacheMatrix etc.
cacheSolve <- function(x, y, ...) {
  if(identical(x$get(), y) == FALSE) {
    message("x$get() and y are different, clearing cache")
    x$setInverse(NULL)
  }
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

unitTestCase <- function() {
  x <- matrix(c(4,2,7,6), nrow=2, ncol=2)
  y <- makeCacheMatrix(x)
  z <- cacheSolve(y, x)
  print(z)
  z <- cacheSolve(y, x)
  print(z)
  m = x %*% z
  print(m)
  y$set(matrix(c(3,2,7,6), nrow=2, ncol=2))
  z <- cacheSolve(y, x)
  print(z)
  m = y$get() %*% z
  print(m)
}

#unitTestCase()
