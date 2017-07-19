## These functions create a matrix object that stores an invertible matix
## Secondly, the inversion is solved and cached

## Creates a special matrix object that stores an invertible matrix 
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list (set = set, get = get,
          setinv = setinv,
          getinv = getinv
          )
}


## This function calculates the inverse of the special matrix created with above function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
      }
      data1 <- x$get()
      m <- solve(data1)
      x$setinv(m)
      m
}
