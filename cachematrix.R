# The objective of the functions is to calculate the inverse of matrix 
# except where the inverse has already been previously calculated.
# Where it has been previously calculated, the value should not be re-calculated and
# the result instead called from the cache

## This function makes a cache that stores the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(mean) m <<- mean
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This solves for the inverse if the inverse has not already been solved for

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getInverse()
   if(!is.null(m)) {
        message("getting cached inverse")
        return (m)
  }
   
   matrix <- x$get()
   m <- solve(matrix, ...)
   x$setInverse(m)
   m
}

