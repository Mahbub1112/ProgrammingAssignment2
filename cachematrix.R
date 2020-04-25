#the following functions create a special matrix and cache its inverse 
#and computes the inverse of the special matrix. If the inverse has 
#already been calculated, the inverse is retrieved from the cache

#makeCacheMatrix() creats an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  g <- NULL
  set <- function(y) {
    x <<- y
    g <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) g <<- inverse
  getinverse <- function() g
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve() uses an argument that is returned by makeCacheMatrix()
#in order to compute and retrieve inverse from the cache 
#value in the makeCacheMatrix() environment

cacheSolve <- function(x, ...) {
  g <- x$getinverse()
  if (!is.null(g)) {
    message("getting cached data")
    return(g)
  }
  data <- x$get()
  g <- solve(data, ...)
  x$setinverse(g)
  g
}