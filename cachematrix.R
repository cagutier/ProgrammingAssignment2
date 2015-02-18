## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
