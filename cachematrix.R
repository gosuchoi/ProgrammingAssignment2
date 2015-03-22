## Two functions below is to calculate inverse matrix passed
## to makeCacheMatrix function. Once the inversion of matrix is
## calculated, the inversed matrix is cached into memory. 
## If the cached inversion matrix is found, the cached matrix will be
## returned instead of recalculating the inversed matrix again.

## This function is to make a list of four functions which
## are related to inversion of matrix. 
##    input : inversible matrix
##    output : list of four functions 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(get=get, set=set, getinverse=getinverse, 
                    setinverse=setinverse) 
}


## This function is to return inversed matrix from the given
## special list returned from makeCacheMatrix().
## If the inversed matrix is found in cache, it will return.
## Otherwise, it will calculate inversed matrx and return it
## after cacheing it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
