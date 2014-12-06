## cachematix.R

## This source file contains two functions. The first creates a special
## matrix object with functions to set and get the matrix as well as to 
## set and get the inverse of the matrix. The second fuction is used to 
## create the inverse of the matrix and store it in the first object, or
## to return the already created inverse from the first object.

## A matrix does not always have an inverse. These functions will only work
## on a square matrix that is inversable. Below is an example of how to use
## these functions with a matrix that is inversable.

## > my_matrix <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## > my_matrix
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > m1 <- makeCacheMatrix(my_matrix)
## > m1$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## > cacheSolve(m1)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > cacheSolve(m1)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1


## makeCaxheMatrix will create the special matrix object. There are 
## four internal functions which set and get the matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ## set will copy the input matrix and set the inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## get will return the stored matrix
    get <- function() x
    
    ## setinverse will store the inverse of the matrix
    setinverse <- function(seti) i <<- seti
    
    ## getinverse will return the stored inverse matrix
    getinverse <- function() i

    ## return a list of the four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix from the speical matrix
## object passed to the function. If the inverse already exists, return it.
## If the inverse does not exist, create and then return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## First see if one exists in the cache
    i <- x$getinverse()

    ## If it does then return it and exit
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    ## If the inverse is not in the cashe then create it
    ## First get the matrix stored in the makeCacheMatrix object
    data <- x$get()

    ## Then use solve() to create the inverse and store it in 
    ## the makeCacheMatrix object
    i <- solve(data, ...)
    x$setinverse(i)
    
    ## Return the inverse
    i
}
