##    Two functions here:
##
##    makeCacheMatrix -- given a matrix, returns a list of methods to get/set the matrix and the inverse of said matrix.
##    cacheSolve -- calls solve() for the matrix, returning the cached value, if available.


# return list of getter/setters for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  
  #cache the inverse in the parent environment.
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  
  #list of methods to get/set the matrix and the solved matrix.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


## given an object created by makecachematrix(), return the inverse of the matrix, using the cached value if available.
cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)){
      message("returning cached value...")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
