## The following R code computes the inverse of a matrix.

## makeCacheMatrix() creates a special object having the methods:get,set,getinverse,setinverse. this function also stores its cached data.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
    inv<<- inverse
  getinverse <- function()
    inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve function computes inverse of a matrix or returns the cached data of the already calculated inverse of that matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv
}
