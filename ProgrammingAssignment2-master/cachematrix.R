# Function to create a special matrix object with caching capabilities

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL # Initialize the cache
  
  set <- function(newValue) { # Function to set the matrix
    x <<- newValue
    cache <<- NULL  
  }
  
  get <- function() x # Function to get the matrix
  
  setInverse <- function(xx) { # Function to cache the inverse of the matrix
    cache <<- inverse
  }
  
  getInverse <- function() cache # Function to retrieve the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function to compute the inverse of the special matrix and cache it

cacheSolve <- function(x, ...) { # Retrieve the cached inverse if available

  cachedInverse <- x$getInverse()
  
  if (!is.null(cachedInverse)) {
    message("Retrieving cached inverse")
    return(cachedInverse)
  }
  
  mat <- x$get() # If not cached, compute the inverse
  inverse <- solve(mat, ...)
  
  x$setInverse(inverse) # Cache the computed inverse
  
  inverse
}
