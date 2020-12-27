##put comments here that give an overall description of what your
##function do

##There are two functions makeCacheMatrix,cacheSolve
##makeCacheMatrix consists of set,get,setinv,getinv
##library(MASS) is used to calculate inverse for non square as well as square matrices
library(MASS)
makeCacheMatrix<- function(x=matrix())
{
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x   #function to get matrix
  setinv<- function(inverse) inv<<-inverse
  getinv<-function()
  {
    inver<-ginv(x)
    inver%*%x     #function to obtain inverse of the matrix 
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

##Write a shor comment describing this function
##This is used to get the ccache data 

cacheSolve<- function(x,...) ##gets cache data
{
  inv<- x$getinv()
  if(!is.null(inv))   #checking whether inverse is NULL
  {
    message("Getting cache data!")
    return(inv)     #return inverse value
  }
  data<-x$get()
  inv<-solve(data,...)    #calculates inverse value
  x$setinv(inv)
  inv  #return a matrix that is the inverse of 'x'
}
