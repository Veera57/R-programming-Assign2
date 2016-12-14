## Programming Assignment 2
## Caching the inverse of a matrix
  
## Creating a cache matrix having set, get, setinv and getinv as list of function input
  
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(a){
        x <<- a
        inv <- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv =setinv, getinv = getinv)
  }
  
  
## Return inverse of matrix from cache if present. Else the inv is calculated
 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
   inv
 }
