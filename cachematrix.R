## makeCacheMatrix will return a list including four functions which can:
#  1 set the value of the matrix
#  2 get the value of the matrix
#  3 set the value of the inversion
#  4 get the value of the inversion

## The following function cacheSolve calculates the inversion of the special "matrix" created 
#  with the above function. However, it first checks to see if the inversion has already been 
#  calculated. If so,it gets the inversion from cache and skips the computation. Otherwise,
#  it calculates the inversion of the data and sets the value of the inversion in the cache  
#  via the setinverse function.

##########################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}


###################################################################

