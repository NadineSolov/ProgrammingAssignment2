##Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##Example with data
my_matrix <- makeCacheMatrix(matrix(16:19, 2, 2))
>  my_matrix$get()
     [,1] [,2]
[1,]   16   18
[2,]   17   19
>  my_matrix$getInverse()
NULL
>  cacheSolve(my_matrix)
     [,1] [,2]
[1,] -9.5    9
[2,]  8.5   -8
>  cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,] -9.5    9
[2,]  8.5   -8
>  my_matrix$getInverse()
     [,1] [,2]
[1,] -9.5    9
[2,]  8.5   -8
