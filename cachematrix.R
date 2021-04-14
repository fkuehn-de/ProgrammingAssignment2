## A pair of functions that cache the inverse of a matrix

## creates a special "matrix" object that can cache its inverse. Comes with 4 functions to get/set the
## original matrix as well as its inverse

makeCacheMatrix <- function(x = matrix()){
  inv<- NULL
  set<-function(A){
    x <<- A
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(B) inv<<- B
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
} 


## Return a matrix that is the inverse of 'x'. If the inverse has been already calculated before, then
## the cached data is used to save computation time.

cacheSolve <- function(x, ...) {
  
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  A<- x$get()
  B<- solve(A, ...)
  x$setinverse(B)
  B
}
