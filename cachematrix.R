## The following functions caclulate the inverse of a matrix and cache the results using R scoping rules 
## The intent is to perform the expensive invers operation once per matrix inverted
## functions do

## Example 
## A <- matrix( c(2, 4, 3, 1, 5, 7), nrow=2,ncol=3, byrow = TRUE)
## cachedMatrix <- makeCacheMatrix(A)

## INV <- cacheSolve(cachedMatrix) # first call

## INV <-cacheSolve(cachedMatrix) # second call returns cached result from previous call
## "getting cached data"


## Create a function that calulates the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y  ## x must exist in the global (calling) env
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse  ## this calulates the inverse
  getinverse <- function() inv   ## return the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Solve for the inverse but only the first time called
## Subsequent calls return the cached result
cacheSolve <- function(x, ...) {
  ## Return a the inverse matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
