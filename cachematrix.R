## The below 2 functions creates special Matrix with ability to cach its 
## inverse

## The first function, `makeCacheMatrix` creates a special "Matrix", which is
## really a list containing a function to :
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  minv<-NULL
  set<- function(y)
  {
    x<-y
    minv<-NULL
  }
  get<- function() x
  setinv<- function(Pinvmatrix) minv<<-Pinvmatrix
  getinv<- function() minv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
    
}


## The following function calculates the inverse of the special "Matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv<-x$getinv()
    if (! is.null(minv)) {
      message("Getting cached data")
      return(minv)
    }
    data=x$get()
    minv<-solve(data,...)
    x$setinv(minv)
    minv
}
