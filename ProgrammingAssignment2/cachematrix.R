## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special Matrix which is a list containing function 
## to set the value of matrix, get the value of matrix, set the inverse of the 
## matrix and get the value of the inverse. This is similar to the example for 
## mean.

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set<-function(y){
              x <<- y
              m <<- NULL
          }
          get <- function() x
          setmatrix <- function(solve) m <<- solve
          getmatrix <- function() m
          list(set = set, get = get,
               setmatrix = setmatrix,
               getmatrix = getmatrix)
}


## Write a short comment describing this function
## This function calculates the inverse of the special matrix created by the
## function above. The cache is checked before computing the matix inverse.
## If the inverse is not available, it is computed using "solve" and sets the
## inverse matrix in the cache.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
         ## solve() - compute the inverse of the matrix
         m <- x$getmatrix()
         if(!is.null(m)){
                message("getting the inverse from cached data")
                return(m)
         }
         matrix <- x$get()
         m <- solve(matrix, ...)
         x$setmatrix(m)
         m
}
