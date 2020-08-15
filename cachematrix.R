## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inversed <<- inv
  getinv <- function() inversed
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- x$getinv()
  if(!is.null(inversed)){
    message("getting cached data")
    return(inversed)
  }
  mat <- x$get()
  inversed <- solve(mat, ...)
  x$setinv(inversed)
  inversed
}
