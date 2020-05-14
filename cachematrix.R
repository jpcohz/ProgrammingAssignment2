## Programming Assignment 2
## These scripts create a Cached Matrix Representation and calculate its inverse,
## caching the resulting inverted matrix into the original matrix representation.



## makeCacheMatrix creates a data structure capable of storing a matrix and
## the necessary functions to set, get its contents.
## It also stores the inverse matrix that initially is NULL, but can be set
## in the future.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix = NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(invMat) inverseMatrix <<- invMat
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}





## cacheSolve takes as input a makeCacheMatrix data structure and calculates (updates)
## its inverse.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inMat <- x$getInverseMatrix()
  if(!is.null(inMat)) {
    message("getting cached data")
    return(inMat)
  }
  matrix <- x$get()
  inMat <- solve( matrix )
  x$setInverseMatrix(inMat)
  inMat	
}

## Example of usage:
## Create a Matrix.
## A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
## det(A)
## The determinant is 16 > 0, therefore an inverse exists.
##
## cach the matrix A:
## A1 <- makeCacheMatrix( A )
##
## test the vector was stored:
## A1$get()
## 
## calculate the Matrix Inverse and store it:
## cacheSolve( A1)
##
## Check that the inverse matrix has been overided and updated with the calculated inverse:
## A1$getInverseMatrix()

