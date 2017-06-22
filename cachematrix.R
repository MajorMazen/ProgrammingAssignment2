
## makeCacheMatrix
# creates a special "matrix", which is
# really a list containing functions to: set and get the matrix value, as well as set and get the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv) 
}


## cacheSolve
# calculates the inverse of the special "matrix"
# created with makeCacheMatrix. It first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) #assumption is that the matrix is always invertible (square and with determinant!=0) 
                    #argument b is identity 
  x$setinv(inv)
  inv
}
