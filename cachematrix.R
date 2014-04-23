# The objective of the functions is to calculate the inverse of matrix 
# except where the inverse has already been previously calculated.
# Where it has been previously calculated, the value should not be re-calculated and
# the result instead called from the cache



makeCacheMatrix <- function(x = matrix()) {
  ## This function makes a cache that stores the value of the inverse
  ## and creates procedures to set and get the Inverse/Matrix
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


## This solves for the inverse if the inverse has not already been solved for

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getInverse()
   
   # If previously calculated inverse of matrix 'x', re-call it form the cache
   if(!is.null(m)) { 
        message("getting cached inverse")
        return (m)
  }
   
  # otherwise, solve for the inverse of the matrix and store the result
   matrix <- x$get()
   m <- solve(matrix, ...)
   x$setInverse(m)
   m
}

