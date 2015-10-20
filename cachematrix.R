## function makeCacheMatrix is used to store a matrix
## and cache its inverse
## assume matrix is always invertible
makeCacheMatrix <- function(cacheMatrix = matrix()) {
  invMatrix <- NULL
  ## sets maxtrix to specified newMatrix
  setMatrix <- function(newMatrix) {
    cacheMatrix <<- newMatrix
    invMatrix <<- NULL  ## since we have a new matrix, set inverse to null
  }
  getMatrix <- function() cacheMatrix  ## returns the cached matrix
  setInv <- function(InvMat) invMatrix <<- invMat  ## sets the inverse matrix
  getInv <- function() invMatrix  ## returns the inverse matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)  ## properties
}


## this function returns the inverse of the specified matrix
## expects a cacheMatrix object
cacheSolve <- function(cacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- cacheMatrix$getInv()
  ## see if inverse exists
  ## if yes, just return cached inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if we get to here than we have to solve ourselves
  data <- cacheMatrix$getMatrix()
  m <- solve(data, ...)
  ## save the inverse in case we need it later
  cacheMatrix$setMatrix(m)
  return(m)
}

## sample usage:
## x <- matrix(1:4, nrow = 2, ncol = 2)
## temp = makeCacheMatrix(x)
## y = cacheSolve(temp)
## View(y)
