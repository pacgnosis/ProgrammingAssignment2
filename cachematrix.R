## Thse functions can be used to find the inverse of a matrix. If the 
## inverse is cached (i.e., it has already been computed), the cached result 
## is used instead of finding the inverse again.


## This function creates a list with a function for setting and getting a
## cacheable matrix and for setting and getting the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function () x
      setInverse <- function(solve) inv <- solve
      getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks if the setInverse() function of makeCacheMatrix has been 
## called for x; if it has, it returns the cached value. If it hasn't, it 
## determines the matrix's inverse, calling setInverse() and returning the inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {    ## If there's cached data for 'x', return it.
            message("Using cached data instead of calculating again.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)   ## Find the inverse of 'x'.
      x$setInverse(inv)
      inv         ## Return a matrix that is the inverse of 'x'.
}
