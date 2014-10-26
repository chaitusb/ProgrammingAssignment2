##Cache the inverse of a matrix

##Function to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##Setting the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##Getting the matrix
  get <- function() x
  ##Getting the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  ##Getting the inverse of the matrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Function to compute the inverse of the special "matrix" returned by above makeCacheMatrix method.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  #Returning the inverse from the cache
  if(!is.null(inv)) {
    message("getting data from the cache")
    return(inv)
  }
  #Calculating inverse if not found in cache
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
