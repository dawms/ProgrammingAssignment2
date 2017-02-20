## Programming Assignment 2: Set and get cached matrix 
## using two functions

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) invmat <<- solve
  getinvmat <- function() invmat
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat = x$getinvmat()
  if (!is.null(invmat)) {
    message("get cached matrix")
    return(invmat)
  }
  inverse_matrix <- x$get()
  invmat <- solve(inverse_matrix, ...)
  x$setinvmat(invmat)
  invmat
}
