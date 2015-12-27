## These two functions work together and cache the inverse of a matrix (a computationally costly operation.
 
 
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
  #figure out how large the matrix is
  d<-dim(x)
 
  s<-matrix(,d[1],d[2])
 
  set <- function(y) {
    x<<-y
    s<<-matrix(,d[1],d[2])
  }
 
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set=set, get=get,
       setinverse=setinverse, getinverse=getinverse)
}
 
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##             If the inverse has already been calculated (and the matrix has not changed),
##                 then the cachesolve should retrieve the inverse from the cache.
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!all(is.na(x))) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
 
 
 