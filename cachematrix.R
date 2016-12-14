## Overall description:
##makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve retrieves the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {        ##initialization of the object matrix x as function argument
  m <- NULL                                        ##m is initialized as object within the makeCacheMatrix() environment and set to NULL,to be used lateron in the function
  set <- function(y) {         ##function for setting the data values within objects.
    x <<- y       ##Assign the input argument to the x object in the parent environment
    m <<- NULL    ##Assign NULL to object m in parent env.When x is reset,the cached value of m is cleared. The inverse can be recalculated and wrong inverses will be avoided. 
  }
  get <- function() x  ##function for retrieving data within an object
  setinverse <- function(solve) m <<- solve ##define the setter for the inverse m: the solvefunction.
  getinverse <- function() m
  list(set=set,     ### gives the name 'set' to the set() function defined above (assign all the set- and getfunctions as elements within a list() and return them to the parent environment).
       get=get,     ## gives the name 'get' to the get() function defined above     
       setinverse=setinverse,  ## gives the name 'setinverse' to the setinverse() function defined above
       getinverse=getinverse)  ## gives the name 'getinverse' to the getinverse() function defined above
} ##By naming the list elements I can use the $ form in the cacheSolvefunction to access these functions by name

cacheSolve <- function(x, ...) {
  m <- x$getinverse()      ##access the getinversefunction in the makeCacheMatrixfunction above
  if (!is.null(m)){        ## if m is not null we get the cached data,
    message("getting cached data") ## we also show a message about this 
    return(m)                  #### and we give back the inverse that was already in the cache 
  }
  data <- x$get()        ##otherwise access the getfunction in the makeCacheMatrixfunction above
  m <- solve(data, ...)  ##the inverse is computed
  x$setinverse(m)        ##access the setinversefunction in the makeCacheMatrixfunction above  
  
  return(m)              ## Return a matrix that is the inverse of 'x'
}
       