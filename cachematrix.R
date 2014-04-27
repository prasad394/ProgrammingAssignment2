## This file contains two functions 
## makeCacheMatrix and cacheSolve that help comupte and retrieve the inverse of the matrix from cache

## maxCacheMatrix takes a matrix as input and gives a vector of functions which set and get the original matrix and set and get the 
## inverse of the matrix. It is assumed that the matrix is square and inversible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function ( y)
  {
    y <<- x
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inv) m <<- solve(x)
  getInv <- function()m
  list (set=set,get=get,getInv=getInv,setInverse=setInverse)    
}

## The cacheSolve function checks if the matrix input is the same as the previous one. If so, it returns the inverse from the cache.
## If not it computes the inverse through the solve function.

cacheSolve <- function(x, ...) {
  # check to see if the matrices have changed
  m <- x$get
  Inv <- x$getInv
  if (x == m) {
  # matrix hasn't changed return the inverse
    message("Taking values from cache")
    retrun(Inv)
  }
  m <- solve(x)  
  x$setInverse(m)
  m
}
