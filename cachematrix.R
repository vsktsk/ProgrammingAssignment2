## A special matrix is created and the inverser of the same is calculated and stored
## in Cache. If the inverse of same matrix is called, the value is retrieved from
## Cache rather than calculating again.

## makeCacheMatrix creates a special Matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
    get <- function() x
    setinv <- function(inverse) inv<<-inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special Matirx returned by makeCacheMatrix.
## If the inverse has always been calculated (and the matrix has not changed) the 
## inverse is retrieved form cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting inverse of matrix")
    return(inv)
  }
  matrix.data <- x$get()
  inv <- solve(matrix.data, ...)
  x$setinv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
