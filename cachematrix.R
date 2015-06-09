## Matrix inversion is often a costly operation computationally.
## In order to address this issue, the functions below create 
## a special "matrix" object and cache the inverse of a matrix 
## rather than calculate it repeatedly:
##
## 1. makeCacheMatrix: This function creates a special "matrix" 
##  object that can cache its inverse.
##
## 2. cacheSolve: This function computes the inverse of the special 
##  "matrix" returned by makeCacheMatrix above. If the inverse has 
##  already been calculated (and the matrix has not changed), then 
##  cacheSolve will retrieve the inverse from the cache.


## This function creates a special "matrix" object, which is
## a list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Replace the object x (in the main function) with y
  ## and set the 'inverse' object to NULL since inverse is 
  ## no longer calculated
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Return the object x
  get <- function() x
  
  ## This function sets the 'inverse' object to x
  setinverse <- function(mat) inverse <<- mat
  
  ## This function returns the 'inverse' object
  getinverse <- function() inverse
  
  ## Store the functions in a list
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix"
## object. It first checks to see if the inverse has already
## been calculated. If so, it gets and returns the cached value 
## and skips the calculation. Otherwise, it calculates, sets and 
## returns the inverse.

cacheSolve <- function(x, ...) {
  ## See if cached inverse exists
  inv <- x$getinverse()
  
  ## If so, indicate so and return the inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise get the matrix object
  mat <- x$get()
  
  ## Calculate its inverse
  inv <- solve(mat, ...)
  
  ## Set the inverse
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
