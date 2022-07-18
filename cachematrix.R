## This function will make a cache of a matrix
#The first function, makeCache creates a special "maytrix", which is really a matrix 
#containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix 


makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the inverse of a matrix of the special 
#"matrix" created with the above function. 
#However, it first checks to see if the inverse matrix has already been 
#calculated. If so, it gets the inverse matrix from the cache and skips the 
#computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the 
#inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
}
