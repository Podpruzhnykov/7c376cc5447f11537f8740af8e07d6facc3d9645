## Function makeCacheMatrix() creates the matrix, where we can remember inversed matrices
## for spending less time on calculation the same data more than once
## Function get() returns original matrix
## Function set() modifies matrix into another one
## Function getinverse() returnes inverse matrix
## Function setinverse() fills matrix as an inversed one #don't call this function without a reason
makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))

  set <- function(y) {
        x <<- y
        m <<- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
     }
  get <- function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returnes inversed matrix. If there was calculation of it before, then the inversed matrix
## will not being calculated once more. Function will return "getting cached data" and the last
## calculation of the data.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!anyNA(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
