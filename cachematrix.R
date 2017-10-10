
## The function makeCacheMatrix creates a special "matrix" type of
## object and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  
  inv <- NULL              
  
  set <- function(y) { 
    
    x <<- y                         
    inv <<- NULL                     
  }
  
  get <- function() x                    
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv     
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}



## The function cacheSolve computes the inverse of the object created in 
## makeCacheMatrix. It will retrieve the inverse from the cache, unless
## the inverse has already been calculated (not being NULL)
 
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
    
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setinverse(inv)
  inv
  
}
