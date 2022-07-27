## Defines function with input of a matrix and output
## is a list of functions to cache and recall that matrix
makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  ## Ability to reset the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Function to recall original matrix
  get <- function() {
    x
  }
  ## Function to set the inverse variable
  setInv <- function(z) {
    inv <<- z
  }
  ## Function to recall the inverse variable
  getInv <- function () {
    inv
  }
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}
## Defines a function that first checks to see if the inverse
## matrix is cached; if it is cached it recalls the inverse,
## if it is not cached it computes the inverse and stores it.
cacheSolve <- function(x) {
  inv <- x$getInv()
  ## Checks if inverse is already stored
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Solves inverse matrix
  inv <- solve(x$get())
  x$setInv(inv)
  inv
}
