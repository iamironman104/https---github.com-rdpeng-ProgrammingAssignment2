makeCacheMatrix <- function(myMatrix = numeric()) {
  m <- NULL
    #This creates an object of type list, set to NULL
  set <- function(y) {
    myMatrix <<- y
    m <<- NULL
  }
    #This function sets the entries of our matrix
  get <- function() myMatrix
    #This function prints the contents of our matrix
  setsolve <- function(inverse) m <<- inverse
    #This function sets the content of our inverse matrix
  getsolve <- function() m
    ##This function prints the contents of our inverse matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
    #Values of our list are the four functions we have just defined
}
cacheSolve <- function(myMatrix, ...) {
  m <- myMatrix$getsolve()
    #myMatrix$getsolve() will be NULL unless we have already computed the inverse of this matrix and stored it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
    #Tests if it is NULL - if not, we already have all of our information
  data <- myMatrix$get()
  m <- solve(data, ...)
  myMatrix$setsolve(m)
  m
    #Here we actually compute the inverse matrix, if necessary
}

