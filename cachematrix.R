## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## Subject: creates a cacheMatrix object
## Input argument:
##  - Receives as argument a matrix object x. It is supposed that it has a inverse matrix.
## Output:
##  - Object with the following structure:
##    Attributes: 
##     - private x: original matrix
##     - private im: calculated inverse matrix for the original matrix x
##    Methods:
##     - set(y): sets a new matrix value to attribute x. 
##     - get(): return the x original matrix
##     - setinvmatrix(im): assigns a matrix to attribute im representing thus the inverse matrix
##     - getinmatrix(): returns the value of inverse matrix im

makeCacheMatrix <- function(x = matrix()) {
  # im holds the cache inverse matrix of the given one x
  im <- NULL
  
  # x holds the original matrix
  # the following method updates the matrix stored in this object and resets the
  # inverse matrix variable
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  # return the original matrix stored by this R object
  get <- function() x
  
  # set/get of the im attribute
  setinvmatrix <- function(invmatrix) im <<- invmatrix
  getinvmatrix <- function() im
  
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## cacheSolve 
## Subject: compute the inverse matrix of the given cacheMatrix create with 
## function makeCacheMatrix. It only computes the inverse matrix when it has not been
## calculated before for the given cacheMatrix or when its original matrix has been
## changed 
##
## Input argument:
##  - cacheMatrix object created with function makeCacheMatrix
## Output
##  - matrix object representing the inverse matrix of the original x matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinvmatrix()
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  # retrieve the original matrix
  data <- x$get()
  # compute its inverse matrix
  im <- solve(data)
  x$setinvmatrix(im)
  im
}
