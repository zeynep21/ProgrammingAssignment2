## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix and returns a 4-element vector. All elements of the vector are functions,
## where you can change the assigned matrix, ask which matrix is assigned, set its inverse, and ask what is the     
## current inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  setmtx <- function(y){
    x <<- y
    inv <<- NULL
  }
  currmtx <- function ()
    x
  setinv <- function(invinp){
    inv <<-  invinp  
  }
  currinv <- function()
    inv
  list(setmatrix=setmtx, currentmatrix=currmtx, setinverse=setinv, currentinverse=currinv)
}


## Write a short comment describing this function
## This function takes the vector created by the previous function, and gives out the inverse of the 
## matrix. If inverse is already set, it will give that out; if not available, it will calculate the 
## inverse, and set it in the vector.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$currentinverse()
  if (!is.null(inv)){
    message ('getting cached data')
    return (inv)
  }
   
  data <- x$currentmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
