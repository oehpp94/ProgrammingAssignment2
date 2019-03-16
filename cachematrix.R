## R Programming - Week 3 - Assignment 2



## This first function returns a "matrix" object that can cached 
## its inverse through the following 4 functions: 
## 1. set        : sets matrix value 
## 2. get        : gets matrix value
## 3. getinverse : sets matrix inverse
## 4. getinverse : gets matrix inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function returns the inverse of the matrix by 
## checking whether the inverse in cached first. If not, it 
## calculates and returns it.

cacheSolve <- function(m, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv
}
