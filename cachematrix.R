## Similar to the makeVector function provided in the course assignment, this function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Similar to the cachemean function provided in the course assignment, this function finds the inverse to the above
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
  return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
