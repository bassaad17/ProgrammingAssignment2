## Caching the Inverse of a Matrix
## Matrix inversion is a difficult computation and it can be made simpler
## by caching the inverse of a matrix instead of computing it repeadtedly
## There are two functions below that are used to create a special object
## that stores a matrix and caches its inverse
## B Assaad

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function (y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     
     list (set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special matrix above created by
## makeCacheMatrix. If the inverse has been calculated (and it is still the
## same), then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
             message("getting cache data")
             return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
