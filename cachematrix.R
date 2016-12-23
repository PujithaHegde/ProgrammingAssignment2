## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. The following 2 functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        invMatrix <- NULL
     ## Set the value of the matrix
     setMatrix <- function(y) 
     {
          x <<- y
          invMatrix <<- NULL
     }
     ## Get the valu of the matrix
     getMatrix <- function() 
          x
     ## Set the inverse of the matrix
     setInverse <- function(inverse) 
          invMatrix <<- inverse
     ## Get the inverse of the matrix
     getInverse <- function() 
          invMatrix
     
     list(setMatrix = setMatrix, getMatrix = getMatrix, 
          setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
     
     ## If the invMatrix is not NULL, then return the cached data from makeCacheMatric function
     if (!is.null(invMatrix)) 
     {
          message("getting cached data")
          return(invMatrix)
     }
     
     ## If the invMatrix is NUUL, then calculate the inverse of the given matrix
     mat <- x$getMatrix()
     invMatrix <- solve(mat, ...)
     x$setInverse(invMatrix)
     invMatrix
}
