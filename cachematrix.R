# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    # if set() is called, assume that the original matrix is updated
    # and invalidate the cache
    if(identical(x, y) == FALSE) {
      x <<- y
      message("updating original matrix, cache flushed")
      i <<- NULL
    }
    else {
      message("new matrix is same as old, ignoring")
    }
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
cacheSolve <- function(x, ...) {
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
  z <- cacheSolve(y)
  print(z)
  z <- cacheSolve(y)
  print(z)
  m <- x %*% z
  print(m)
  y$set(x)
  z <- cacheSolve(y)
  y$set(matrix(c(3,2,7,6), nrow=2, ncol=2))
  z <- cacheSolve(y)
  print(z)
  m <- y$get() %*% z
  print(m)
}

# uncomment test case and source file to run
# unitTestCase()
