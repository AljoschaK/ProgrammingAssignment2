## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix will create a special matrix.
## In fact it is a list of functions to
## 1. set the values of a matrix
## 2. get the values of a matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        
    }
    get <- function() x
    setinverse <- function(inverse) m <<-inverse
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Write a short comment describing this function
## the cacheSolve function takes the makeCacheMatrix function to compute the 
## inverse of a matrix. However if the inverse has already been computed and 
## saved in the cache it will return the cached values instead.
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