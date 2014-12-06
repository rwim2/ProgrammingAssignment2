## cachematix.R

## This source file contains two functions. The first creates a special
## matrix object with functions to set and get the matrix as well as to 
## set and get the inverse of the matrix. The second fuction is used to 
## create the inverse of the matrix and store it in the first object, or
## to return the already created inverse from the first object.

## A matrix does not always have an inverse. These functions will only work
## on a square matrix that is inverable. Below is an example of how to use
## these functions with a matrix that is inversable.

## > my_matrix <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## > my_matrix
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > t1 <- makeCacheMatrix(my_matrix)
## > t1$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > cacheSolve(t1)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > cacheSolve(t1)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1

## makeCaxheMatrix will create the special matrix object. There are 
## four internal functions which store the matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(seti) i <<- seti
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will create the inverse of the matrix in the speical matrix
## object if it does not exist or return it from the object if it does.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
