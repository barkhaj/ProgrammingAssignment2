## Function accepts a valid matrix as a parameter
## returns inverse of matrix
## Uses the Solve function to invert a matrix
## Assumes a matrix is always invertible, only works with square matrixes

## Usage Example
## my_matrix<-matrix(rnorm(1:16),4,4)
## ds<-makeCacheMatrix(my_matrix)
## cacheSolve(ds)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  
  list(setMatrix = setMatrix, getMatrix = getMatrix
       , setSolve = setSolve, getSolve = getSolve)
  
}


## Function returns the inverse value of a matrix. 
## Value is cached for future references.
cacheSolve <- function(x, ...) {
  
    #Check the matrix is square
    if(ncol(x$getMatrix())==nrow(x$getMatrix()))
      {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        
          #If value exists retrieve from cache
          if(!is.null(m)) {
            message("Getting cached data...")
            }
          
          #Else calculate inverse value
          else{
                data <- x$getMatrix()
                m <- solve(data, ...)
                x$setSolve(m)
              }
      }
    else{
         message("Not a square matrix, cannot be inverted...")
         m<-NULL
        } 
 
    return(m)
 
}
