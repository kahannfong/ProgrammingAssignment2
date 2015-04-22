## Put comments here that give an overall description of what your
## functions do
## I have put comments in line in function instead to describe what each function does.

## Write a short comment describing this function
## The makeCacheMatrix function creates a list of 4 functions described as below:
## 1. sets the value of the matrix - this is the matrix x that will be fed into the function makeCacheMatrix(x)
## 2. gets the value of the matrix - it displays the matrix x
## 3. calculates the value of the inverse of matrix x
## 4. gets and returns the inverse of matrix x

makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    xInverse <<- NULL
  }#function to set value of the matrix
  get <- function() x #get the matrix printed
  setinverse <- function(x) xInverse <<- solve(x) #calculate and set the inverse of x
  getinverse <- function() xInverse #get inverse of x
  list(set=set,get=get,setinverse=setinverse(x),getinverse=getinverse) #display as list
}


## Write a short comment describing this function
## Note that cacheSolve takes as an argument the result of makeCacheMatrix, but canNOT accept as an argument the original matrix x
## in cacheSolve, if xInverse (the inverse of x) exists in the cache, it returns the saved value and does not compute further.
## However, if xInverse has not been saved in cache, it will calculate the inverse of matrix x (as input)

cacheSolve <- function(y, ...) {
  xInverse <- y$getinverse()#Renamed argument as y to prevent confusion with previous argument x for makeCacheMatrix.
  if(!is.null(xInverse)) {
    print(message("getting cached data"))#chose to print this as well, to identify if result comes from cache. 
    #So this function calls the matrix xInverse as defined in previous function, if it exists. No new calcs.
    return(xInverse)#xInverse has been defined in makeCacheMatrix
  }
  data <- y$get()#If xInverse does not exist in cache, then xInverse is calculated using the solve() function.
  xInverse <- solve(data, ...)
  y$setinverse(xInverse)
  xInverse#returns matrix inverse of X as calculated.
}
