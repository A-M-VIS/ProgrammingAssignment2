## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "Matrix" , ehich is really a list 
## containing a function to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse
## 4. get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve calculates the inverse of the "Matrix" created with 
## makeCacheMatrix, it first checks if the inverse has already been calculated/
##If so it gets the values from the cache and skips the computation, if not
## then calculates the inverse of the matrix and sets the values of the inverse 
## in the cache using setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
