## makeCacheMatrix creates a special matrix object that can cache its inverse
## calculate inverse of matrix
## If inverse of matrix already calcuated, find in cache, return, do not calculate again
 

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
}

  get <- function () x
  setinverse<- function (inverse) inv_x <<-inverse
  getinverse <- function () inv_x
  list (set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## cacheSolve returns inverse of matrix created with makeCacheMatrix
## If cache inverse already calculated, it should retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
