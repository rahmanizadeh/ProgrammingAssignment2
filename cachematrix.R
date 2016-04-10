##        This program calculte and cache the inverse of a matrix 

## The makeCacheMatrix function, creates a special "Matrix", which is really a list containing a function to
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the inverse of the Matrix
## 4. get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)


}


## The cacheSolve function return a matrix that is the inverse of 'x'
## This inverse matrix is calculated or returned from cach

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv       

}
