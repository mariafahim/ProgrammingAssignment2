## these functions allow for efficient matrix inversion and caching
## of these results 

## this function first inverses a matrix and then caches the result 

makeCacheMatrix <- function(x = matrix()) {
  l <- NULL
  set <- function(y) {
    x <<- y 
    l <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) l <<- solve
  getinverse <- function() l
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function first checks to see if the previous function cached the inverse
## of the matrix, and retrieves it from there
## if the inverse is not available, this function computes it 

cacheSolve <- function(x, ...) {
  l <- x$getinverse()
  if(!is.null(l)) {
    message("getting cached data")
    return(l)
  }
  data <- x$get()
  l <- solve(data, ...)
  x$setinverse(l)
  l
}



