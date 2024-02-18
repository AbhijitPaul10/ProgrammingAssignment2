## Put comments here that give an overall description of what your
## functions do
## Fuction is able to cache potential time-consuming computations.

## Write a short comment describing this function
## This function "makecachematrix" will matrix to cache it's inverse
makeCacheMatrix <- function(x = matrix()) { ## Argument default mode is matrix
## "inv" will hold value of matrix inverse
  inv <- NULL
## "set" assigns new value of matrix to the "inv" as it resets to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
## "get" Returns the value of the matrix argument
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse ##assigns value of "inv"
  getinverse <- function() inv  ## gets the value of "inv"
## "List" help you create a list to refer the function using $ operator
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## Write a short comment describing this function
## "cacheSolve" computes the inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## If the inverse has already been calculated
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
