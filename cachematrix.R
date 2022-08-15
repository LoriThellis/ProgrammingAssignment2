## The below functions will cache the inverse of a matrix
## The Usage of these funtcions are to pass the result of a makeCacheMatrix call to cacheSolve

## The first function will create a special Matrix to: 
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setvalue <- function(y) { 
    x <<- y
    inv <<- NULL
    }
  getvalue <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The below function computes the inverse of the special "matrix" returned by makeCacheMatrix above

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
}
