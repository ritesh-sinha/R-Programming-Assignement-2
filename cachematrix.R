## The function first create a cache matrix and define setters and getters for
## setting the matrix and its inverse in cache. It then checks if inverse exist 
## in the cache or not. In case it does not exist it calculates the inverse and 
## set it in the cache so that next time when it calculate, it directly get it 
## from the cache.

## The function includes setters for the matrix and its inverse and store
## them in cache. It also include getters to retrieve these values if required.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## THis function calculate the inverse using solve() function in case the inverse 
## does not exist in cache else it retrieve it directly from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data ..")
    return(inv)
  }
  inputmatrix <- x$getmatrix()
  inv <- solve(inputmatrix, ...)
  x$setinverse(inv)
  inv
}
