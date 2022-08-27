## A) Purpose: Create a low resource data cache for use by a repeated request
## B) Function: Two functions set within an enclosed environment, subset
## from the global environment, to create and set a matrix, invert (solve(x))
## the matrix, do a save state query and retrieve (with message) if cache is
## detected or, if no cache is detected, and create one for reference access
## by global environment, without needing to create the enclosed environment
## each time the functions are needed.

## Get, set, invert matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Check cache save state (!NULL=load, message) || (NULL=save)

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("Fetching")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}