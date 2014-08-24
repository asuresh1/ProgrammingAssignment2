## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix as an argument.
## The function creates a superassignment.
## Tells R to calculate the inverse of a matrix.
## This function stores the inverse of the matrix in memory
## 
makeCacheMatrix <- function(x = matrix()) {
	  inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_m <<- solve
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of R but first checks if the inverse
## of the matrix is in memeory or cache. If the inverse is not in memory the
## function calculates the invesre. If the inverse of the matrix is available
## the function retrives the data from memeory and hence the expensive 
## computation required for inverse is prevented.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinverse(inv_m)
        inv_m

}

