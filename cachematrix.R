## The function makeCacheMatrix performs the work of storing a matrix and its inverse in the
## defining environment of the function. The setter and getter functions for the matrix and
## its inverse are then stored as elements of a list object
## The function cacheSolve is the function that maintains state for the cached variables and accesses
## the cached matrix and its inverse when previously computed

## This function creates a special "matrix" object that also caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed i.e. the referce variables are pointing to the same object),
## then the cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  data <- x$get()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
