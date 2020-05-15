## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set function inputs the matrix and stores it and inverse in a parent environment,
## get function retreives the matrix
## setinv function calcutates the inverse and is retreived by getinv 

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y) ##set matrix
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x ##get matrix
  setinv<-function(solve) inv<<-solve  ##set inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##If the inverse is already in cache, it returns that. ELse, the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinv(inv)
  inv
}
