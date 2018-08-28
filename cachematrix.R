## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions
## which can be used to create and manipulate
## a special matrix which has a cached inverse
## which is i

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve takes a special matrix created by the
## makeCacheMatrix method and calculates its
## inverse, it first checks if the inverse has 
## already been calculated, if so its get the
## inverse and skips the computation otherwise
## it calculates the inverse and sets the
## value of the inverse via the setinverse
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

