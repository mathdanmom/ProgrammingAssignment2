## These functions will cache the inverse of a matrix,
## so we don't recalculate the inverse of the same matrix again.

## makeCacheMatrix creates four functions

makeCacheMatrix <- function(x = matrix()) {
  stored_inverse <- NULL
  set <- function(y){
    x <<- y    #the scope of these assignments is one level
    stored_inverse <<- NULL   # up, the parent environment
  }
  get <- function() {
    x
  } 
  setinverse <- function(some_inverse) {
    stored_inverse <<- some_inverse
  }
  getinverse <- function() {
    stored_inverse
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Retrieve inverse from cache, or calculate it if it isn't there

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  #calculating the inverse
  x$setinverse(inv)
  inv      ## Returns a matrix that is the inverse of 'x'
}
