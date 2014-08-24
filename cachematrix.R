## The function makeCacheMatrix creates a makeCacheMatrix object which caches the often resource intensive  computation of calculating an inverse.
## It contains a getter/setter for both the matrix itself, and the inverse computation
##
##The function cacheSolve, computes the inverse of matrix x. 
##If the inverse is cached it will immdiately return that, otherwise it will compute and then cache that value.

## The makeCacheMatrix object contains 4 functions
## The setter assigns the matrix value to the arguemtn and invalidates the inverse value
## The getter retuns the stored value of the matrix
## The setinverse sets a predetermined inverse value
## The getInverse returns the stored inverse value

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseVal) inverse <<- inverseVal
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the cached inverse matrix of makeCacheMatrix mx, or if not cached, computes and caches that value.
## It first checks if the value is caches, if it is, notify the user with a message and return the cached value.
## If not, compute it, store it in makeCacheMatrix mx's inverse field and using its setter and return that value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
