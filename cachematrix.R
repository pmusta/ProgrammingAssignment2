

# This function is used with cacheSolve; it caches 
# inverse of matrix "x" sent by cacheSolve.

makeCacheMatrix <- function(x = matrix()) { 
      inverse <- NULL #variable inverse is set to null
      
      # set function can be used to set within makeCacheMatrix
      # x to a certain matrix, and inverse to be NULL (not really needed?)
      set <- function (y){
            x <<- y
            inverse <<- NULL
      }
      
      
      get <- function() {
            x 
      }#returns x (x within makeCacheMatrix) to caller
      
      setInverse <- function(inv) {
            inverse <<- inv
      } # inv is inverse from cacheSolve, this makes inverse 
      # sent from cacheSolve inverse in makeCacheMatrix environment
      
      getInverse <- function() {
            inverse
      } # return cached value of inverse to cacheSolve
      
      #list  returns a labeled vector of functions
      list(get = get, set = set, setInverse = setInverse, 
           getInverse = getInverse)
      
}



# this function computes the inverse of the matrix x, and caches the
# result to makeCacheMatrix.
# If the inverse has already been calculated 
# then the cachesolve should retrieve 
# the inverse from the makeCacheMatrix.

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()  #get cached value of inverse x
      
      
      #if there is a cached value of inverse x (inverse != NULL), print msg
      #and return the value (the function ends there)
      
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
      #otherwise (inverse = NULL) continue to calculate the inverse x
      data <- x$get() #get x from makeCacheMatrix and set it to "data"
      inverse <- solve(data, ...) #solve inverse of data and set it to "inverse"
      
      x$setInverse(inverse) #send inverse to be cached 
      
      inverse # return the inverse
}
