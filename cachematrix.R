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
makeCacheMatrix <- function(x = matrix()) {
    l<-length(x)   #return the length of x
    numrow=sqrt(l) #square root of length of matrix values, to be used as nrow parameter
    numcol=sqrt(l) #square root of length of matrix values, to be used as ncol parameter
    mod=sqrt(l)%%1 #calculate modulus of the length of matrix values (if it's 0 or not)
    if (mod!=0){print("Error:square matrix can not be created with the values you supplied")}
    #if mod is not 0, inform user the value supplied can not be converted to a square matrix
    else {
      invm<-NULL
      mx<-matrix(x,numrow,numcol) ## if mod is 0 then go ahead and create 
      setmx<-function(y){mx<<-y   ## a matrix called mx
      invm<<-NULL}
      getmx<-function()mx
      setinvm<-function(mx)invm<<-solve(mx) # set the inverse of matrix mx
      getinvm<-function()invm
      list(setmx = setmx, getmx = getmx,
           setinvm = setinvm,
           getinvm = getinvm)
                              }
                                            }

## Write a short comment describing this function
    cacheSolve <- function(x, ...) {
    
    invm <- x$getinvm()
    if(!is.null(invm)) {
      message("getting cached data")
      return(invm)
    }
    
    data <- x$getmx()
    
    invm <- solve(data, ...)
    x$setinvm(invm) ## Return a matrix that is the inverse of 'x'
    invm
  }
