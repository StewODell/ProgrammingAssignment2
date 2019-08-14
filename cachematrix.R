## Put comments here that give an overall description of what your
## functions do

## sets matrix value, clears inverse cache; gets matrix and inverse values; sets inverse value from cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    mat <<- y
    minv <<- NULL
  }    
  get <- function() mat    
  setinv <- function(invrs) minv <<- invrs
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  }


## solves for inverse if none cached; returns cached inverse if cache value exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
  }
