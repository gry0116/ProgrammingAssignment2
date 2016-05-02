## The pair of functions below cache the inverse of a matrix

## Create a special "matrix", which is a list containing a 
## function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function calculates the inverse of a special "matrix" 
## created with the above function. If the inverse has already been
## calculated, the function should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      matrix<- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i
}

