## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initialize the inverse property
  
  ## Define a function to set the matrix
  set <- function(y) {
    x <<- y  # Assign the input matrix to the variable x in the parent environment
    inv <<- NULL  # Reset the inverse property since the matrix has changed
  }
  
  ## Define a function to get the matrix
  get <- function() x  # Return the matrix x
  
  ## Define a function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse  # Assign the input inverse to the variable inv in the parent environment
  
  ## Define a function to get the inverse of the matrix
  getInverse <- function() inv  # Return the inverse property
  
  ## Return a list of the functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  # Retrieve the inverse from the cache
  
  ## Check if the inverse is already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  ## If the inverse is not yet calculated, compute it
  data <- x$get()  # Get the matrix from the object
  inv <- solve(data, ...)  # Calculate the inverse of the matrix
  
  x$setInverse(inv)  # Cache the calculated inverse
  inv  # Return the calculated inverse
}
