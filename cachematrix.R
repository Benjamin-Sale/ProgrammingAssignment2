## Creates a special matrix which caches its inverse
makeCacheMatrix <- function(x = matrix()) {

## Create the inverse object
i <- NULL

## Set the matrix
set <- function(matrix) {
  x <<- matrix
  i <<- NULL
}

##Get matrix
get <- function() x

##Set inverse of matrix
setinverse <- function(inverse) i <<- inverse

##Get inverse of matrix
getinverse <- function() i

##lists methods 
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## This function computes the inverse of the matrix created by the "makeCacheMatrix" function. 
## The function first check whether the invese has already been calculated and if so, the inverse will be retrieved from the cache. 

cacheSolve <- function(x, ...) {
  
  ##Return inverse of matrix 'x'
  x <- x$getinverse ()
  
  ##Check if the inverse has aleady been calculated
  if(!is.null(x)) {
    message("getting cached data")
    return(x)
  }
  
  ##get the invese from cache 
  data <- x$get()
  
  ##Calculate the inverse using solve function
  x <- solve(data)
  
  #set inverse to the matrix 'x'
  x$setinverse(x)
  
  ##return matrix
  x

}
