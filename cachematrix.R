
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 ## Initialize the inverse property
  m<-NULL  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    m <<- inverse
  }
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## to test : 
## > mx = makeCacheMatrix(matrix(c(4,2,7,6),nrow=2,ncol=2))
## > imx = cacheSolve(mx)
## > imx = cacheSolve(mx)
## getting from cached data
## note : the message "getting from cached data" only appears on the 2nd cachSolve.
## imx is the inverse matrix of mx
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting from cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse
        m <- solve(data) 
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}
