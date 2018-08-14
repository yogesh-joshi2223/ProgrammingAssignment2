## Calculates the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## Set function
  set <- function(var){
    x <<- var
    inverse <<- NULL
  }
  ## Get function
  get <- function() x
  ## SetInv function
  setInv <- function(mat) inverse <<- mat
  ## GetInv function
  getInv <- function() inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  ## Calculates the determinant of a matrix
  det_val <- det(mat)
  ## Checks the condition of determinant value
  if(det_val == 0) {
    print("Determinant is Zero. Inverse of a matrix doesn't exist")
    x$setInv(0)
  }
  else {
  ## Assigning the inverse of a matrix using solve function and assigning it to inverse variable
  inverse <- solve(mat)
  x$setInv(inverse)
  }
  print(inverse)
}
