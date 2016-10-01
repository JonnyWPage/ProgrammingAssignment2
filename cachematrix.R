makeCacheMatrix <- function(x) {
  ## x is a matrix with ncol>2 and nrow>2
  
  ## makeCacheMatrix creates a matrix object that can
  ## cache its own inverse
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## cacheSolve returns a matrix that is the inverse of 'x'
  
  ## If the inverse of 'x' has already been calculated
  ## cacheSolve does not compute the matrix, and instead
  ## returns the inverse of 'x' that has already been calculated
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
