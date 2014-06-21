# Inverse matrix:
#
# The intention with the two functions below - makeCacheMatrix and cacheSolve
# - is to calculate the inverse of a matrix (the inverse is assumed to exist)
# If the inverse of the given matrix already have been calculated the inverse
# recovered from the cache.


makeCacheMatrix <- function(matrix=matrix()) {
  #This function creates a special "matrix" object that can cache
  #its inverse.

  Inverse<-NULL

  getMatrix<-function() matrix

  setInverse<-function(InverseMatrix) Inverse<<-InverseMatrix

  getInverse<-function() Inverse

  list(getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
}


cacheSolve <- function(x, ...) {

#  This function computes the inverse of a special "matrix" returned
#  by the makeCacheMatrix function. If the inverse has already been calculated
# (and the matrix has not changed), then the cacheSolve function will retrieve the
# inverse from the cache.


  Inverse <- x$getInverse()

  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }

  data <- x$getMatrix()

  Inverse <- solve(data, ...)

  x$setInverse(Inverse)

  Inverse
}