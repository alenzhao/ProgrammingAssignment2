 ##    makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

 ##    cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
                ## If the inverse has already been calculated (and the matrix has not changed),
                ## then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. 
   ## For example, if "X" is a square invertible matrix, then "solve(X)" returns its inverse.
   ## For this assignment, assume that the matrix supplied is always invertible.

## Write a short comment describing this function

## "makeCacheMatrix" contains 4 functions: "setM", "getM", "setIM", "getIM".

##  "setM" sets the matrix stored in the main function.
##  "getM" returns the matrix x stored in the main function.

##  "setIM" sets the inverse matrix stored in the main function.
##  "getIM" sets the inverse matrix stored in the main function.


makeCacheMatrix <- function(X = matrix()) {
    
  IM <- NULL
    
    setM <- function(Y) {
      X <<- Y
      IM <<- NULL
      ## use "<<-" to assign a value to an object
        ## in an environment different from the current environment. 
      
    }
    
    getM <- function() X
    
    setIM <- function(solve) IM <<- solve
    ## use "<<-" to assign a value to an object: 
        ## in an environment different from the current environment. 
    
    getIM <- function() IM
    
    list(setM = setM, 
         getM = getM,
         setIM = setIM,
         getIM = getIM)
  }

## Function “cacheSolve” computes the inverse of inversable matrix "X". 
  
  ## if the inverse matrix "IM" has already been calculated, returned by makeCacheMatrix above. 

  ## if the inverse has not been calculated, "getIM" calculates the inverse, and "X$setIM(IM)" stores 
    ## it in the object "IM" in "makeCacheMatrix".


cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
  
  IM <- X$getIM()
  
  if(!is.null(IM)) {
    message("getting cached inverse matrix")
    return(IM)
  }
  
  data <- X$getM()
  IM <- solve(data, ...)
  X$setIM(IM)
  IM
}
