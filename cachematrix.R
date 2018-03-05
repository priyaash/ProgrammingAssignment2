
##Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## 
##this function creates a matrix that can cache uts inverse.
makeCacheMatrix <- function(x = matrix()) {
 inv<- NULL
 set<- function(y){
   x<<- y
   inv<<- NULL
 }
  get<- function()x
  setinverse <- function(inverse)inv <<- inverse
  getinverse <- function()inv
  list(set = set,
       get = get ,
       setinverse = setinverse,
       getinverse = getinverse )
}


## This function computes the MakeCacheMatrix inverse.
## if the matrix is not changed and its inverse is already been calculated
##then it will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(matrix(data, nrow = rows, ncol = cols), ...)
  x$setinv(inv)
  inv
}

