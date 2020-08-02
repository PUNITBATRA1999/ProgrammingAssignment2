## here function input is x , which is a matrix of which we are calculating inverse. here j is the environment where we are cachaing the inverse of the matrix x
##makeCacheMatrix is storing the cache of the matrises

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##cacheSolve is a function which calculates the returns the inverse of the matrix 
##first it search for inverse in the cache if its not there, it calculate the inverse and then save it in the j for further use

cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of x
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
