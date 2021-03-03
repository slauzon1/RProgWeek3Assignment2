## Coursera R Programming Week 3: Assignment 2 Lexical Scoping

##Function makeCacheMatrix March 2, 2021 
## This function creates a matrix that can have its inverse cached
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Function cacheSolve March 2, 2021 
## Will creates the inverse of the matrix from function makeCacheMatrix. If the inverse has already
## been solved it will return the inverse from the cache with the message "Getting cached data"
cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()
  
  if(!is.null(inv))
  {
    message("Getting cached data")
    return(inv)
  }
  data  <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
