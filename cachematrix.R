## I copy the example functions
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## &
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} # This function compute and cache the matrix and its inverse (if the matrix can be inverted). This is specially useful when changes to the original matrix are made, as it allows to compute that 'new inverse matrix' without having to change again the original matrix


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
} # This function computes the inverse of a special matrix created by makeCacheMatrix. It retrieves the inverse from cache if already calculated and the matrix remains unchanged; otherwise, it computes and caches the inverse for future use

## Example: I create a 3x3 matrix with the squares of the first positive numbers (excluding 0 and 1)

squaredmatrix <- matrix(c(4, 9, 16, 25, 36, 49, 62, 81, 100), 3, 3)
squaredmatrix_cached <- makeCacheMatrix(squaredmatrix)
cacheSolve(squaredmatrix_cached)
