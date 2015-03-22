
## Caching the inverse of a matrix        


## The "makeCacheMatrix function creates a special matrix that is a list that contains 
## the functions to 
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) ## inpit a matrix
{
  mi <- matrix(numeric(0),0,0) ## assigning a "0 x 0" matrix to the matrix
  ## that will store the inverse of input matrix
  set <- function(y) 
  {
    m <<- y
    mi <<- matrix(numeric(0),0,0) 
    ## resets the inverse to a "0 x 0" matrix
    
  }
  get <- function() m ## returns the input matrix
  
  setsolve <- function(solve) mi <<- solve 
  ## it will store the inverse matrix when function "cacheSolve" is first called
  
  getsolve <- function() mi
  ## in all subsequent calls of the function "cacheSolve" for the same input matrix,
  ## this will return the cached inverse matrix of the input matrix
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  ##to access the functions in the list externally, this list is created
}


##The "cacheSolve" function sees if the inverse of the matrix exists in cache and 
##returns it. If inverse of the input matrix is not present in the cache, it creates
## the inverse and stores in the cache

cacheSolve <- function(m, ...) 
{
  mi <- m$getsolve() 
  ## Return a matrix that is the inverse of 'x' if already present in cache
  ## if not, return the "0 x 0" matrix
  
  if(nrow(mi) > 0) 
    ##if "mi" is not a "0 x 0" matrix then the next steps in the flower brackets
    ##are executed
    
  {
    message("getting cached data") ##print the message
    return(mi) ## return the inverse matrix from cache
  }
  
  else
    
  {
    data <- m$get() 
    ## if not in cache, then create matrix "data" with input matrix
    
    mi <- solve(data) ## calculate inverse of the matrix "data"
    
    m$setsolve(mi) ## Cache the calculate inverse matrix
    
    mi ## return the inverse matrix
  }
}



a<-matrix(12:15,2,2) ##example "2 x 2" matrix

a ## show the input matrix "a"

b<-makeCacheMatrix(a) 

c<-cacheSolve(b) ## store inverse of the input matrix in matrix "c"

c ## return the inverse matrix "c"
