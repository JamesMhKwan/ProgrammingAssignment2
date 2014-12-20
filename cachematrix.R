## makeCacheMatrix explanation
## (1) Sets the inversed matrix object to NULL
## (2) function to returns the original matrix
## (3) function to stores Inversed Matrix calculated in cacheSolve
## (4) function to return cached Inverse matrix
## (5) Returns list of all three functions (2,3,4)


makeCacheMatrix <- function(x = matrix()) {
InvrsdMtrx <- NULL  ## (1)
GtMtrx <- function() {x} ## (2)
StInvrsdMtrx <- function(csInvsrdMtrx) { InvrsdMtrx <<- csInvsrdMtrx} ## (3)
GtInvrsdMtrx <- function() {InvrsdMtrx} ## (4)
list(GtMtrx = GtMtrx,  StInvrsdMtrx=StInvrsdMtrx,GtInvrsdMtrx=GtInvrsdMtrx ) ## (5)
}


## cachceSolve Explanation
## (1) Gets the cached inverse matrix object
## (2) If (1) is not NULL, returns inversed matrix
## (3) If (1) is NULL, gets original matrix
## (4) Calculate inverse of original matrix
## (5) Cache inverse of original matrix
## (6) Return inverse of original matrix

cacheSolve <- function(x, ...) {
  
  InvrsdMtrx <- x$GtInvrsdMtrx() ## (1)
  
  if (!is.null(InvrsdMtrx)) { ## (2) 
    message("getting cached data")
    return(InvrsdMtrx) 
  }
  Mtrx <- x$GtMtrx() ## (3) 
  InvrsdMtrx <- solve(Mtrx) ## (4)
  x$StInvrsdMtrx(InvrsdMtrx) ## (5)
  InvrsdMtrx ## (6)
}
