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

#############################################################################################################################
################################################## Testing the Code #########################################################

#### Example 1 ####

## Inverse Matrix is not set to any value but set to NULL and hence cacheSolve function calculates the inverse of the matrix
## which is given as an input to the makeCacheMatrix function
             
> source("cacheMatrix.R")
> myMatrix <- makeCacheMatrix(matrix(c(7,13,15,17,32,43,2,4,8), 3, 3))
> myMatrix$getMatrix()
     [,1] [,2] [,3]
[1,]    7   17    2
[2,]   13   32    4
[3,]   15   43    8
> myMatrix$getInverse()
NULL
> cacheSolve(myMatrix)
      [,1] [,2] [,3]
[1,] -42.0   25 -2.0
[2,]  22.0  -13  1.0
[3,] -39.5   23 -1.5
> myMatrix$getInverse()
      [,1] [,2] [,3]
[1,] -42.0   25 -2.0
[2,]  22.0  -13  1.0
[3,] -39.5   23 -1.5

#### Example 2 ####

## Inverse Matrix is set to some value in the makeCacheMatrix function and hence cacheSolve function retreives the cached 
## data (inverse matrix value from the makeCacheMatrix function)
             
> source("cacheMatrix.R")
> myMatrix$setMatrix(matrix(c(7,13,15,17,32,43,2,4,8), 3, 3))
> myMatrix$getMatrix()
     [,1] [,2] [,3]
[1,]    7   17    2
[2,]   13   32    4
[3,]   15   43    8
> myMatrix$setInverse(matrix(c(-42, 22, -39.5, 25, -13, 23, -2, 1, -1.5), 3, 3))
> myMatrix$getInverse()
      [,1] [,2] [,3]
[1,] -42.0   25 -2.0
[2,]  22.0  -13  1.0
[3,] -39.5   23 -1.5
> cacheSolve(myMatrix)
getting cached data
      [,1] [,2] [,3]
[1,] -42.0   25 -2.0
[2,]  22.0  -13  1.0
[3,] -39.5   23 -1.5
