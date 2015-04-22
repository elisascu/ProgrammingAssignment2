## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                   # inv is NULL, it will store the cached inverse matrix
      set <- function(y) {          # setter for the matrix
            x <<- y                 
            inv <<- NULL            
      }

      get <- function() x           # getter for the matrix
      
      setinv <- function(inverse) inv <<- inverse     # setter for the inverse
      getinv <- function() inv                        # getter for the inverse
      list(set=set, get=get, setinv=setinv, getinv=getinv)  # list with the functions we just created
      
      }


cacheSolve <- function(x, ...) {    # to calculate the inverse of a matrix, if the same inverse has been already calculated, then returns the cached one
     
      inv <- x$getinv()             # test
      if(!is.null(inv)) {                       # if inv is not null, then we get the cached one
            message("getting cached data")
            return(inv)                         # the output is the cached inverse matrix
      }
      
      data <- x$get()                           # if the inverse is not calculated yet
      inv <- solve(data,...)
      
      x$setinv(inv)
      
      inv                                       # the output is the new calculated inverse matrix
      
      
}
