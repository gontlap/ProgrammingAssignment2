## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invM= NULL
  set <-function(y){
    x<<- y
    invM <<- NULL
  }
  get<-function() x
  getInverse <-function() invM
  setInverse <-function(inv) {
    invM <<-inv
  }
  
  list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)
  
}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInverse()
  if( !is.null(inv)){
    message("return cached data")
    return(inv)
  }
  #else compute and store
  tempdata <-x$get()
  x$set(tempdata)  
  inv<-solve(tempdata,...)
  x$setInverse(inv)
  inv
}
