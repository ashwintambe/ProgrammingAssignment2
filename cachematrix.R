## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{

  inv = NULL
  set = function(y) 
  {
      x <<- y
      inv <<- NULL
  }
  get = function() x
  setInv = function(inverse) inv <<- inverse 
  getInv = function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        
   inv = x$getinv()
        
   # if the inverse has already been calculated
    if (!is.null(inv)){
         # get it from the cache and skips the computation. 
             message("getting the cached data")
             return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
