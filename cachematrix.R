##'makeCacheMatrix' creates a special "matrix" object that can cache its inverse. 
##cacheSolve returns the matrix' inverse and it inputs it into the special vector
##defined in the first function.

##This 'special matrix' is a list of four functions: 
##the first one set the matrix if it wasn't done when defining the 'makeCacheMatrix' function.
##the second one returns the matrix
##the third one is the one used by the cacheSolve function when setting the inverse values
##it takes the solution 'sol' given by cacheSolve  and assigns it to 'inv'
##the fourth one returns the inverse if it was previously calculated.
makeCacheMatrix<-function(mat=matrix()){
  inv<-NULL
  setmat<-function(y){
    mat<<-y
    inv<<-NULL
  }
  getmat<- function() mat
  setinv<-function(sol) inv<<-sol
  getinv<- function() inv
  list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv)
}

#If the inverse was previously calculated,it shows a special
#message and retrieves the value 'inv' from 'makeCacheMatrix'
cacheSolve<-function(mat,...){
  inv<-mat$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-mat$getmat()
  inv<-solve(data,...)
  mat$setinv(inv)
  inv
}
