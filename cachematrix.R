## These functions allow the user to cache the inverse of a function and recall it for later use 
##without having to recalculate the function

## This function creates a series of functoins to set the value of a matrix, get the value of the matrix
##set the inverse of a matrix, and then get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## This functoin gets a stored inverse matrix value, determines whether the value has been set yet
## gets the matrix data, and then calculates the inverse, setting the inverse to be cached, and then
##returning the inverse value

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
