## The first function, makeCacheMatrix, creates a special matrix object 
## that can cache its inverse. The second function, cacheSolve, calculates 
## the inverse of the matrix retuned by the first function using the solve 
## function. If the inverse has already been calculated, cacheSolve will 
## retrieve the inverse from the cache and print a message saying it is doing so.

## The makeCacheMatrix creates contains four functions: set, get, setsolve, 
## and getsolve. The set function changes the matrix x stored
## in the main function to y; the get function returns the matrix x 
## stored in the main function; setsolve stores the value of the input in a 
## variable m into the main function; getsolve returns the value stored in m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The cacheSolve function below calculates the inverse of the matrix returned by the
## makeCacheMatrix function. It checks to see if the inverse matrix has 
## already been calculated and, if it has, will retrieve the inverse from 
## the cache and print the message "Getting cashed inverse of square matrix." 
## Otherwise, it calculates the inverse of the matrix with the solve function 
## and sets it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ##Checks if the inverse matrix has already been calculated and, if it has, 
  ##retrieves the inverse from the cache and prints message
  if(!is.null(m)) {
    message("Getting cached inverse of square matrix")
    return(m)
  }
  ##Otherwise, data gets matrix stored in makeCacheMatrix, calculates the 
  ##inverse of the matrix with the solve function, and sets it in the cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}