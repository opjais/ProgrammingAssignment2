## The overall function is used to set inverse matrix into cache ##if not done. If already stored cache matrix is there, then just ##returns it without doing computation
## 

## makeCacheMatrix defines a function which calls functions such as set for setting data, get for getting stored data, setinvm for setting inverse matrix, getting stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvm <- function(solve) m <<- solve
  getinvm <- function() m
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}



## cacheSolve is a function, which first checks if there is any stored inversmatrix using x$getinvm(). If yes, then it returns that inverse matrix. Otherwise, it gets stored data by x$get() and computes inversematrix using solve() and sets inverse matrix into cache using x$setinvm() and returns inverse matrix

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinvm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvm(m)
  m
}