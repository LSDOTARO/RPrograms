## Assignment: Caching the Inverse of a Matrix
## Writing a pair of functions that cache the inverse of a matrix.
## namely; (1) makeCacheMatrix and (2) cacheSolve

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inversematrix <- NULL
      
      ## Set the value of the matrix
      set <- function(y) {
             x <<- y
             inversematrix <<- NULL
      }
      
      ## Get the value of the matrix
      get <- function() x
      
      ## Set the value of the inverse matrix
      setinverse <- function(inverse) inversematrix <<- inverse
      
      ## Get the value of the inverse matrix
      getinverse <- function() inversematrix
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
      inversematrix <- x$getinverse()
      if (!is.null(inversematrix)) {
              message("getting cached data")
              return(inversematrix)
      }
      data <- x$get()
      inversematrix <- solve(data, ...)
      x$setinverse(inversematrix)
      ## Return a matrix that is the inverse of 'x'
      inversematrix
      
}
