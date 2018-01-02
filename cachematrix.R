## followed similar path given in the example
## used concepts of lexical scoping & parent env assignment operator 

## makeCacheMatrix function return a S3 object with a list of named functions: set, get, setinv, getinv 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invr) inv <<- invr
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##this either extracts the already cached inverse or computes the inverse in case it's not cached

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
