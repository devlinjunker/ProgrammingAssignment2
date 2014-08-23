## Caching Matrix Mean
## -------------------
## Functions for matrix container that caches the inverse of the matrix
## for performance reasons

## Creates and returns the matrix container that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function(){
    x
  }
  
  set <- function(y = matrix()){
    x <<- y
    inverse <<- NULL
  }
  
  getInverse <- function(){
    inverse
  }
  
  setInverse <- function(i = NULL){
    inverse <<- i
  }
  
  list(get = get, set = set, 
        getInverse= getInverse, setInverse = setInverse)
}


## returns the inverse of the matrix container provided, if the
## container has a cached value, returns that instead of solving

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
          return(i)
        }
        
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInverse(i)
        i
}
