#### makeCacheMatrix - 
## it is confusing to name these functions this way, given the way the examples are named i.e. why not just name it "makeMatrix". 
## these functions take a matrix, and then store it, along with functions to be used in the process of solving for it's inverse and 
## and then caching things to be used instead of repeating the calculation

## sets the matrix to a value, set() resets based on new value if necessary, get() gets the matrix, setinver() sets the inverted matrix
## and getinver() calls the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(solve) inver <<- solve
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## uses the functions made in makeChacheMatrix to determine if a solution was made/cached and, if so, returns that solution (along with
## the message that it is doing so). if a solution was not cached then it gets the matrix, inverts it, and sets it as the inverse, then 
## returns it. 

cacheSolve <- function(x, ...) {
inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
