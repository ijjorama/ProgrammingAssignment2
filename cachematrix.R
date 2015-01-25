## Solution for Programming Assignment 2

## 'makeCacheMatrix' - wrap a matrix with a caching method

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {   
    x <<- y
    inv <<- NULL   
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## 'cacheSolve' - calcuate the inverse of a matrix wrapped by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
    
  inv <- x$getinverse()   ## See if there is already an inverse cached  
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  data = x$get()          ## No inverse yet, so calculate and cache it
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
