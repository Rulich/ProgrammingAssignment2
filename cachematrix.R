## Put comments here that give an overall description of what your
## functions do

## ####################################
## makeCacheMatrix 
## Creates a list of functions to store and
## retrieve matrix.
## 
## it can be considered as a matrix object with 4 methods:

## set(m) - applies a new matrix m and clears cache
## get() - gets stored matrix 
## setCache(n) - saves matrix n to cache
## getCache() - retrieves matrix from cache

##
## usage example:
##
## object constructor
## mx<-makeCacheMatrix(matrix(c(1,1,2,3),2,2))
##
## mx$set(matrix(c(1:16),4,4)))
## mx$get()
## mx$setCache(matrix(c(1:9),3,3))
## mx$getCache()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(imx) m <<- imx
  getCache <- function() m
  list(set=set,
       get=get,
       setCache=setCache,
       getCache=getCache
      )
}

## ####################################
## cacheSolve
## calculates inverse of matrix object created by makeCacheMatrix function

## tries to get inverted matrix from cache 
## if there is no cached object yet, then inverts given as a parameter object
## and saves it in the cache

##
## usage example:
## mx<-makeCacheMatrix(matrix(c(1,1,2,3),2,2))
## cacheSolve(mx)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m  
}






