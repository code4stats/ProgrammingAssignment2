## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This `makeCacheMatrix` function creates a vector (really a list) containing 
## functions that:
## 1. sets the value of the vector
## 2. gets the value of the vector
## 3. set the value for the inverse of the matrix
## 4. get the value for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the matrix passed into
## from the above function. If the inverse has already been calculated, it is 
## retreived from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    #print(data)
    i <- solve(data, ...)
    #print(i)
    x$setinverse(i)
    i
}
