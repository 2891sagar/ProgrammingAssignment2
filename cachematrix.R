## These functions compute the inverse of a square matrix. If the inverse of the matrix 
## already exists then the inverse is returned from the result stored in Cache, else it is 
## computed agian.

## makeCacheMatrix is used to store the inverse of the matrix to decrease computational time.
## The output is a list containing 4 functions which can be used to reset the input matrix, 
## and store the inverse.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse for the matrix already exists in the Cache, if not it 
## computes it and stores in the cache by called the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
