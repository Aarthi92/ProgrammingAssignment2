## R Programming - John Hopskin University via www.coursera.org
## Programming Assignment 2 - Caching Matrix Inverse
##
## makeCacheMatrix: This function creates a special "matrix" 
##                  object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
##                 "matrix" returned by makeCacheMatrix above. 
##                 If the inverse has already been calculated 
##                 (and the matrix has not changed), then the 
##                 cachesolve should retrieve the inverse from 
##                 the cache.


## Sets matrix passed as argument. Sets inverse passed.
## Gets matrix passed. Gets inverse if set, NULL otherwise.
makeCacheMatrix <- function (x = matrix())
{
        inverse <- NULL
        
        #set the matrix passed as argument.
        set <- function(y)
        {
                x <<- y
                inverse <<- NULL
        }
        
        #get matrix set in the environment.
        get <- function () x
        
        #set inverse matrix passed by cacheSolve function below.
        setinverse <- function( inv )
        {
                inverse <<- inv
        }
        
        #get inverse set above.
        getinverse <- function() inverse
        
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse =getinverse)
}



## Solves a matrix to find inverse. If already solved, returns
## cached inverse.
cacheSolve <- function(x,...)
{
        ## fetch if inverse 
        inverse <- x$getinverse()
        
        ## check if inverse is not NULL (meaning, solved already)
        if(!is.null(inverse))
        {
                message("getting cached data")
                return(inverse)
        }
        
        ## if inverse is NULL, solve and set inverse before printing
        inverse <- solve(x$get())
        x$setinverse(inverse)
        inverse
}
