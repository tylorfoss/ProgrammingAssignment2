## @function: makeCacheMatrix
##    enables use of inner functions (get/set/getinverse/setinverse)
## @function: cacheSolve
##    using makeCacheMatrix, assigns and returns the solved result



## return a container that can hold its inverse

makeCacheMatrix <- function(x = matrix()) {

  # immediately set the solve to null
  s <- NULL

  # should be testing is.matrix(x) here to ensure PEBCAK==FALSE
  # but we were told to assume ... meh

  # why aren't we solving for s here, if we're setting?
  # maybe we don't plan on needing the solved answer sometimes?
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## Store the value of the solve if it's not already solved

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s

}
