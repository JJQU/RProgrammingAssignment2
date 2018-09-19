## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# initial inverse property
  i <- NULL
  
  ## set the value of the matrix
  set <- function(y){ 
    x <<- y
    i <<- NULL
  }
  
  ## get the vale of the matrix
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  # generate a list that contains all elements
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above


cacheSolve <- function(x, ...) {
  
  ## set the value of the matrix
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get the value of the matrix
  data <- x$get()
  i <- solve (data) %*% data
  
  ## return a matrix that is the inverse of 'x'
  x$setinverse(i)
  i
}





