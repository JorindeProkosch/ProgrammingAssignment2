## R Programming - Assignment 2
## 
## For large matrices it may take long to compute the inverse of a matrix. 
## If the contents of a matrix are not changing, it may make sense to cache the 
## inverse of the matrix so that when we need it again, it can be looked up in the 
## cache rather than recomputed. 
##
## The functions makeCacheMatrix and cacheSolve are used to cahce the inerse of a matrix.
##
## It is assumed that the matrix is always invertible.
##
## makeCacheMatrix: this functions creates a list whith four functions: 
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() {
    inv
  }
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: this functions returns the inverse of a matrix. First it checks if the inverse already is computed. 
## If so, it returns the result from the cache, without computing the inverse. If there is no already computed inverse
## in the cache, it computes it inverse and sets the value in the cache. 

cacheSolve <- function(x,...){
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
}

## Example: 
## x <- matrix(1:4, 2, 2)
## m <- makeCacheMatrix(x)
## cacheSolve(m)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Here we see that the function cacheSolve is computing the inverse of the matrix instead of getting it from the cache. 
## That is because it had never been in the cache before. However, it will be stored in the cache now. 
##
##cacheSolve(m)
## getting chached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## Now we see that the function cacheSolve gets the cached inverse of the matrix intead of computing it.
