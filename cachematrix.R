## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    # Initialize m to NULL
    m <- NULL
    
    # Setup the set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Setup the get function
    get <- function() x
    
    # Setup the setsolve function
    setsolve <- function(solve) m <<- solve
    
    # Setup the getsolve function
    getsolve <- function() m
    
    # Return the list of functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
    ## Return a matrix that is the inverse of 'x'
  
    ## Get cached inverse matrix
    m <- x$getsolve()
    
    ## If m is not NULL, we have the cached inverse. Return it
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## Otherwise, get the original matrix
    data <- x$get()
    
    ## Calculate the inverse using solve()
    m <- solve(data, ...)
    
    ## Cache the inverse
    x$setsolve(m)
    
    ## Return the inverse
    m
    
}
