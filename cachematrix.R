## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.Follwoing pair of functions facilitate an object, supporting 
## sub functions to efficiently calculate inverse of a matrix 
## (i.e. inverse is calculated only if it is not calculated earlier)

## makeCachematrix creates a special "matrix" object that can cache its inverse.
## This object has four functions.
## 1) get -- getting the data matrix
## 2) set -- setting data matrix same as argument 
## and resetting inverse of data matrix to NULL
## 3) setinverse -- setting inverse of data matrix same as argument
## 4) setinverse -- getting inverse of data matrix

makeCacheMatrix <- function(x = matrix()) {
        # Initialize inverse to NULL
        xInv <- NULL
        
        # get the data matrix
        get <- function() x
        
        # set the data matrix to 'y' and reset inverse to NULL
        set <- function(y){
          x <<- y
          xInv <<- NULL
        }  
        
        # get the inverse of data matrix
        getinverse <- function() xInv
        
        # set the inverse of data matrix to inverse
        setinverse <- function(inverse) xInv <<- inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## and the matrix has not changed (whenever the matrix is reset, 
## inverse is also reset to NULL) then  should retrieve the inverse 
## from the cache. otherwise the inverse is calculated and set. In a nutshell
## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       
        # Get current value of inverse of the data matrix stored in object x
        xInv <- x$getinverse()
        
        # If xInv is not NULL (i.e.  a valid inverse) return the inverse
        # obtained from getinverse function
        if( !is.null(xInv) ){
            message("getting cached inverse of matrix")
            return(xInv)
        }
         
        # If xInv is NULL ( i.e. inverse is not calculated from the time data matrix 
        # is altered) inverse need to be calculated. Access the data matrix and calclate
        # inverse using solve function. Also, store/cache the newly calculated 
        # inverse in object x and return the inverse
        datamat <- x$get()
        xInv = solve(datamat) # Assuming datamat is invertible Square matrix
        x$setinverse(xInv)
        xInv
}
