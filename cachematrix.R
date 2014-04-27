

## this function return tne invert of a given matrix

makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invert) { inv <<- invert}
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheinv <- function(x, ...) {
  inv <- x$getinv()
  if((!is.null(inv)) & (identical(solve(x$get()),inv))) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
