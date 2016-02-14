
## makeCacheMatrix wille create a list with a function. First,, it will set the value of a matrix, then it will get the value of such matrix.
## after that, it will set the value of the inverse of said matrix and lastly, it will get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function (y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve will retunr the inverse of such matrix. Following the example given, it will first check if the inverse has already been
## calculated, if so, it will get the result without the computatio. In other case, it computates the inverse.
cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
