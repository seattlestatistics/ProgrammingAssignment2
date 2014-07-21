## The functions create an inverse of a matrix, and cache it.  They check to see if the matrix exists if a new matrix is created, so as to save time and processing.

## This function takes data in the form of a matrix and caches it.  If the data is not in the form of a matrix, an error will be thrown.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function actually creates the inverse, through the Solve function.  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
