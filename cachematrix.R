## This below two functions helps to cache and retrieve the inverse of matrix whever required!

## The 'makeCacheMatrix' function helps to STORE the matrix and inverse of matrix when it is calculated. 
## This function gets a matrix as input and produces a 'special matrix'.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The 'cacheSolve' function helps to RETRIEVE an already calculated inverse of a matrix or calculate the inverse
## when a new matrix is passed into the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
