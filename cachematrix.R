## These two functions first create a square matrix based on user's input. 
## The inverse of the matrix is then calculated and cached for future use. 
## The next time these functions are called again for the same
## matrix (given the matrix has not changed), the cached value will be returned.
## If a brand new matrix is presented to the functions however, the calculation for 
## the new matrix's inverse is calculated from scratch and then cached.


## This makeCacheMatrix function first examines if the value of x that user supplies is suitable 
## to create a square matrix (otherwise an error message will be return and the function stops).
## If the value of x is appropriate, then a square matrix will be created along with 4 functions to
## set matrix value, get matrix value, set/calculate the inverse of the matrix, and retrieve the 
## inverse of that matrix. 
makeCacheMatrix <- function(x = matrix()) { # x is a set of values to create a square matrix
    l<-length(x)   # Return the length of x
    mod=sqrt(l)%%1 # Calculate modulus of the length of matrix values (if it's 0 or not).  
    if (mod!=0){print("Error:square matrix can not be created with the values you supplied")}
    # if mod is not 0, inform user that the values of x supplied can not be converted to a square matrix
    else { # if mod is 0 then go ahead and create a matrix called mx
      invm<-NULL                  # Set inverse of the matrix to null (to benefit new calculation)
      mx<-matrix(x,numrow,numcol) # create a matrix called mx with the values of x
      setmx<-function(y){mx<<-y   
      invm<<-NULL}
      getmx<-function()mx         # getmx function simply returns the matrix mx
      setinvm<-function(mx)invm<<-solve(mx) # setinvm function calculates the inverse of matrix mx
      getinvm<-function()invm     # getinvm function returns the inverse of mx calculated with setinvm function
      list(setmx = setmx, getmx = getmx, # Create a list containing 4 functions
           setinvm = setinvm,
           getinvm = getinvm)
                              }
                                            }

## cacheSolve function primarily does two things: 
## 1.Check if the inverse of the same matrix has previously been calculated.  
##   If so, return the cached value.
## 2.If the inverse has not been calculated before, then go ahead and calculate the inverse from scratch.
    cacheSolve <- function(x, ...) {
    
    invm <- x$getinvm()               # Retrieve inversed matrix from previous function
    if(!is.null(invm)) {              # If the value of inversed matrix is available (not null),
      message("getting cached data")  # then print message "getting cached data"
      return(invm)                    # and display the cached data (inverse of the matrix)
    }
    
    data <- x$getmx()                 # If the value of inversed matrix is null,
                                      # then calculate the inverse of the matrix from scratch
    invm <- solve(data, ...)
    x$setinvm(invm) ## Return a matrix that is the inverse of 'x'
    invm
  }
