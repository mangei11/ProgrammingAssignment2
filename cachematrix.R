## The two functions generate an object to store a matrix and to cache its 
## inverse. The first function creates a special "matrix" object for that
## purpose. The second function is used to return the inverse of the matrix
## stored in that matrix object, which involves either calculating the inverse
## and storing it for future use, or accessing the previously calculated inverse
## cached in the matrix object

## This function creates a special "matrix" object that can cache its inverse.
## The function makeCacheMatrix sets up a list of functions to 
##      (1) set the value of the matrix
##      (2) get the value of the stored matrix
##      (3) set value of the inverse of the cached matrix
##      (4) get value of the the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y){
               x <<- y 
               xinv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xinv <<- inv
        getinv <- function() xinv
        
        ## complile the list of functions to be returned
        list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## The function below now calculates the inverse of the "matrix" object. The
## matrix is, according to the description of the assignment, assumed to be
## invertible. The function first checks if the inverse had already been 
## calculated, and if not, calculates the inverse. In case the inverse already 
## exists, it is retrieved from the cache without performing any calculations
## but printing a message that the cached inverse matrix is returned.

cacheSolve <- function(x, ...) {        
        xinv <- x$getinv()
        if(!is.null(xinv)){
            message("Retrieving cached inverse matrix")
            return(xinv) 
            ## Returns a matrix that is the inverse of 'x', exits function
        }
        mat <- x$get()
        xinv <- solve(mat,...)
        x$setinv(xinv)
        xinv 
        ## Returns a matrix that is the inverse of 'x'
}
