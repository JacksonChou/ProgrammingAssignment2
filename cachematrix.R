## makeCacheMatrix creates a list containing functions to:
## (1) set matrix
## (2) get matrix
## (3) set inverse of matrix
## (4) get inverse of matrix



makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  
  setinversematrix <- function(inverse) inversematrix <<- inverse
  getinversematrix <- function() inversematrix
  
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If inverse is already calculated then cachesolve should retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inversematrix <- x$getinversematrix()
  
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  
  mat.data <- x$get()
  m <- solve(mat.data, ...)
  
  x$setinversematrix(inversematrix)
  
  return(inversematrix)
  
}
