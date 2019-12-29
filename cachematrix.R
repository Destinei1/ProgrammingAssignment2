## Matrix Get and Caching
## 

## Start with an empty matrix
## Initialize M and set to Null
## to Null if there was a previous Inverse stored

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize M and set to Null
  I <- NULL
  ## Assign input argument to X object in parent environment and set I to Null in case of 
  ## previous Cached inverse
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  ## retrives the value of x from parent environment
  get <- function() x
  ## assigns the input argument to I
  setInv <- function(Inv) I <<- Inv
  ## retriveves the value of I from the parent evironment
  getInv <- function() I
  ## Creates list so we can use $ operator to extract
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)}


## Uses the previous function output functions to set the Inverse to I
## if I isn't NULL then it pulls from Cache
## If I is NULL then it computes the inverse and sets new inverse to I

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
