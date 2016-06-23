
## makeCacheMatrix stores a matrix and its inverse. It creates a list
## of four functions to
##
##   'set' the value of a matrix
##   'get' the value of a matrix
##   'set' the value of the inverse
##   'get' the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function returns the inverse of a matrix. It checks first
## whether the inverse is in the cache. If so, it retrieves and returns
## the result. Otherwise, it computes the inverse (using the solve() 
## function), stores the inverse in the cache and then returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x',
        ## checking the cache first

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Example of how to use these functions
##
## > m <- makeCacheMatrix(matrix(1:4,2,2))  # 2x2 matrix in the makeCacheMatrix type
## > m$get()                                
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m$getinverse()
## NULL
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## Getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > m$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

