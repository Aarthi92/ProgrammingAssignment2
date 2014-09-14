## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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
