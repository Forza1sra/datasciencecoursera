## Put comments here that give an overall description of what your
## functions do
#The following functions are designed to save time on computing the inverse of a matrix. When fed a new matrix, its inverse is calculated and stored.
#if asked to solve the same matrix again, it will speedily return the cached version rather than redoing the same computation again.

## Write a short comment describing this function
#Takes a matrix and will cache its inverted counterpart.
makeCacheMatrix <- function(x = matrix()) 
  {
    invCache <- NULL
    set <- function(y) 
    {
      x <<- y
      invCache <<- NULL
    }
    get <- function() x
    setInv <- function(invMatrix) invCache <<- invMatrix
    getInv <- function() invCache
    list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  }


## Write a short comment describing this function
#Solves the matrix by finding its invervse. If the inverse has already been found, and the matrix hasn't changed, then it will return the cached value
#rather than doing the same computation all over again.
cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$gentInv()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invCache)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInv(invMat)
  invMats
}
