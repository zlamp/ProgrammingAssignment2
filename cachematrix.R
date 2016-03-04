## The first function, makeCasheMatrix creates a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverted matrix
##get the value of the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The following function calculates the inverted matrix created with the above function. 
##However, it first checks to see if the inverted matrix has already been calculated.
##If so, it gets the inverted matrix from the cache and skips the computation.
##Otherwise, it calculates the inverted matrix of the data and sets the value of theinverted matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
 m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
