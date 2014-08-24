## makeCacheMatrix creates a matrix objec
## cacheSolve take the matrix and calculates the inverse of it
## cache is smart enough not to redo calculation that is already done

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
      x <<- y
      m <<- NULL
}
get <-function()x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set=set, get=get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}


## cacheSolve returns the inverse of the matrix that is created 
## with makeCacheMatrix.

cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      if(!is.null(m)){
            message("getting cached inverse data")
            return(m)
      } else {
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      return(m)
      }
        ## Return a matrix that is the inverse of 'x'
}
