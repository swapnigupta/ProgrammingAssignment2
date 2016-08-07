## This function is used to cache the inverse of a matrix. It contains four different fuction  to:
##1. Set the value of matrix -> setfunction
##2. Get the value of matrix -> getfunction
##3. Set the inverse of matrix -> setinverse
##4. Get the inverse of matrix -> getinverse


makeCacheMatrix <- function(x = matrix()) {
  
inversevalue<- NULL
setfunction <- function(y)
{
  x <<- y
  inversevalue <<- NULL
}
getfunction <- function() x
setinverse <- function(inverse) inversevalue <<- inverse
getinverse <- function() inversevalue
list(setfunction=setfunction,getfunction=getfunction,setinverse=setinverse,getinverse=getinverse)
}


## This function is used to find the inverse of a matrix. 
##If the inverseis already present in cache,it just passes back the value, else calculates the inverse and saves in cache.
## solve() function used to calculate inverse of matrix

cacheSolve <- function(x, ...) 
  {
  
  inversevalue <- x$getinverse()
  if(!is.null(inversevalue)) {
    message("getting cached inverse data")
    return(inversevalue)
  }
  data <- x$getfunction()
  inversevalue <- solve(data)
  x$setinverse(inversevalue)
  inversevalue
}
