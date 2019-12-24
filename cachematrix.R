## Matrix Get and Caching
## 

## First I is set to Null 
## Then the next inside function set the current matrix, and sets "I"
## to Null if there was a previous Inverse stored

makeCacheMatrix <- function(x = matrix()) {

  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) I <<- Inv
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  {
    message("Can't compute, Determite is equal to 0")
  }
  I <- solve(data, ...)
  x$setInv(I)
  I
}
