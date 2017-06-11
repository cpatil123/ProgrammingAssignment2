## makeCacheMatrix
# The first function, makeCacheMatrix, creates a special "matrix", which is really a 
# list containing a function to:
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix using the solve() function
# 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve 
  
  #calculates the inverse of the matrix and caches the result
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
# calculates the inverse of the special "vector" using makeCacheMatrix. 
# It first checks if the inverse has already calculated
# If yes, gets the inverse from cache and skips the computation
# Else it calculates the inverse and sets the value of the 
# inverse in the cache using Solve() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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


## Use following to test
# source("cacheMatrix.R")
# a <- makeCacheMatrix(matrix(rnorm(4),nrow = 2,ncol = 2))
# a$get()
# a$getinverse()
# cacheSolve(a)

# a$getinverse()  
# this is only to show you that the mean has been stored and does not affect anything

# cacheSolve(a) 
# second time cachesolve - retrieves from cache after # checking if changed or not

=============== ==============
  
  # Results for your reference:
  #> source('cacheMatrix.R')
  #> a <- makeCacheMatrix(matrix(rnorm(4),nrow = 2,ncol = 2))
  #> 
  #> a$get()
  #           [,1]       [,2]
  #[1,]  0.8606162  0.9438155
  #[2,] -2.2779166 -0.8628701
  #> a$getinverse()
  #NULL
#> cacheSolve(a)
#           [,1]       [,2]
#[1,] -0.6131243 -0.6706412
#[2,]  1.6186053  0.6115228
#> 
#> a$getinverse()
#           [,1]       [,2]
#[1,] -0.6131243 -0.6706412
#[2,]  1.6186053  0.6115228
#> 
#> 
#> cacheSolve(a) 
#getting cached data
#           [,1]       [,2]
#[1,] -0.6131243 -0.6706412
#[2,]  1.6186053  0.6115228

