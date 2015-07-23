## These functions are for creating and manipulating matrices
## with the added feature of caching its inverse.

## makeCacheMatrix:
##   Create a matrix with the added feature of having a
##   cached inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      # Define functions to set or access the matrix itself, 
      # re-initializing the inverse cache when it is no longer
      # valid
      set <- function( newx ) {
            x <<- newx
            inv <<- NULL
      }
      get <- function() x
      
      ## Define functions to set the inverse cache (calculated elsewhere)
      ## and get a cached inverse 
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      
      # Return list of functions
      list( set = set, get = get, 
            setinv = setinv,
            getinv = getinv)
}


## cacheSolve:
##   Given a cacheMatrix, return its inverse. If the 
##   matrix hasn't changed since the last call, this 
##   simply returns the cached inverse.

cacheSolve <- function(x, ...) {
      
       xinv <- x$getinv()
       if ( !is.null(xinv) ) {
             message("Retrieving cached inverse.")
             return(xinv)
       }
       xinv <- solve(x$get())
       x$setinv(xinv)
       
       xinv
}
