## The first function makeCacheMatrix creates a "matrix", and caches
## the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
InvrsdMtrx <- NULL  ## sets Invrsd Matrix ro NULL
GtMtrx <- function() {x} ## returns the original matrix
##will store invesred matrix calculated by cacheSolve
StInvrsdMtrx <- function(csInvsrdMtrx) { InvrsdMtrx <<- csInvsrdMtrx} 
GtInvrsdMtrx <- function() {InvrsdMtrx} ## returns cached inversed matrix

list(GtMtrx = GtMtrx,  StInvrsdMtrx=StInvrsdMtrx,GtInvrsdMtrx=GtInvrsdMtrx )
}


## cachceSolve: takes inverse matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  InvrsdMtrx <- x$GtInvrsdMtrx() ## gets cached inversed matrix
  
  if (!is.null(InvrsdMtrx)) { #if cachced inversed matrix is not NULL (i.e. it exists)
    message("getting cached data")
    return(InvrsdMtrx) ## return the existing cached inversed matrix
  }
  ## Else...
  Mtrx <- x$GtMtrx() ## gets original matrix (not inversed)
  InvrsdMtrx <- solve(Mtrx) ##perform inverse calculation
  x$StInvrsdMtrx(InvrsdMtrx) ## cache cacluated inverse matrix
  InvrsdMtrx ## returned calcualted inverse matrix
  
  
  ## Return a matrix that is the inverse of 'x'
}
