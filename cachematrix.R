#  R programming assignment 2
# Write the following functions:
# 1.  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2.	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) 
 {
   inv <- NULL
   set <- function(y) 
          {
            x <<- y
            inv <<- NULL 
          }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

 }


## Write a short comment describing this function
# The following function returns the inverse of the matrix created with the makeCacheMatrix function. 
# It first checks if the inverse has already been computed. 
# If inverse matrix is computed already, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.

# This function assumes that the matrix is always invertible.
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
  {
        
    inv <- x$getinverse()
    if(!is.null(inv)) 
      {
        message("getting cached data.")
        return(inv)
      }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  
  }
# Sample output
#> x <- matrix(data=1:4,nrow =2, ncol=2)
#> x
#[,1] [,2]
#[1,]    1    3
#> m <- makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(m)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
#> cacheSolve(m)
#getting cached data.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
