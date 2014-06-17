## Functions makeCacheMatrix and cacheSolve are intended to reduce cost of 
## repeated inverse matrix calculation.
## Usage example
## matrix <- makeCacheMatrix(sample(1:100, 9), 3, 3)
## inverse <- cacheSolve(matrix) #first launch - inverse matrix calculated 
##  by solven
## inverse2 <- cacheSolve(matrix)# second launch - cached value returned

## Function returns list representation of invertible matrix. List members
## (in order of appearance):
## matrix - source matrix
## setinverse - function, that should be used to set value of matrix 
## inversion 
## getinverse - function, that should be used to get inverted matrix 
## representation

makeCacheMatrix <- function(x = matrix()) {
  matrix <- x
  inverse <- NULL
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(matrix = matrix, setinverse = setinverse, getinverse = getinverse)
}


## Function return inverted matrix of "invertible" matrix x. In case of 
## repeated usage inverted matrix value cached

cacheSolve <- function(x, ...) {
    if(is.null(x$getinverse())) {
        #print("solving")
        #"..." allow to pass additional parameters to solve
        x$setinverse(solve(x$matrix,...))
    } else {
        x$getinverse()
    }
}
