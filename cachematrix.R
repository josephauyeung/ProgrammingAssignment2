## cachematrix.R
## Coursera R Programming
## 2015-4-26 Chiho AU-YEUNG
## Programming Assignment 2
## https://class.coursera.org/rprog-013/human_grading/view/courses/973494/assessments/3/submissions

## This function creates a special vector that caches the calculation of its inverse
## It does this by creating the object as a list with `set` and `get` functions,
## and some clever scoping magic from R
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## The inverse starts out as NULL
  
  ## By setting the object, I assign the new argument to be my stored value and
  ## reset my inverse calculation
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Returns my internal object
  get <- function() x
  
  ## Get and Set functions for the inverse, as above
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will calculate the inverse of the received square matrix, caching
## the result in the CacheMatrix object.
## In the cache already exists, it will simply return the cached result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)  ## Calculating the inverse
  x$setInverse(inv)
  inv
}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
