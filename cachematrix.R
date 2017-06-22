
## makeCacheMatrix
# creates a special "matrix", which is
# really a list containing functions to: set and get the matrix value, as well as set and get the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  set <- function(y) {
    x <<- y      #cache the matrix 
    inv <<- NULL #clearing the inverse cache
  }
  
  get <- function() x #return the matrix data
  setinv <- function(inverse) inv <<- inverse #cache the inverse
  getinv <- function() inv #return the inverse
  list(set = set, get = get, #create a list 
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
  if(!is.null(inv)) { #if cache isn't null, retrieve the value from the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   #otherwise, get the matrix data
  inv <- solve(data) #compute the inverse
                    #assumption is that the matrix is always invertible (square and with determinant!=0) 
                    #second argument in solve is identity (default)
  x$setinv(inv)
  inv #return the inverse as an output to the function
}
