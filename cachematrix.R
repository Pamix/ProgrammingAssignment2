## These two functions cache the inverse of a matrix

## makeCacheMatrix creates a matrix that can cache its inverse. If x is not a matrix it 
## will stop. If the matrix is not square, the solve function will print an error message

makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) stop("x must be a matrix")
  inverse <- NULL
  set <- function(y=matrix()) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.  
## First it will check if the inverse has already been calculated. If so, it will get it from
## the cache. Otherwise it will calculate the inverse and set the value in the cache 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  print(inverse)
}
