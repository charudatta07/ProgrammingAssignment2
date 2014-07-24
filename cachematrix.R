##Assuming that the matrix supplied is always invertible

## Below are two functions that are used to create a special object that stores a matrix and cache's its Inverse


##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the matrix inverse has already been calculated. If so, it gets the inverse of the matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  ##Check whether Cache Data Exists, if yes then return the result from cache
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  #Else Perform the Computation and return the result
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

