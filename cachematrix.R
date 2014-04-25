#'Make  a matrix and support methods for setting and getting the data and caching its inverse
#'get the inverse of the matrix
#'
#'@author Harvinder Singh
#'
#'@param x is an object of type matrix the default value if 1x1 matrix with value 0
#'@return makeCacheMatrix object
makeCacheMatrix <- function(x = matrix(0, nrow=1, ncol=1))
{ 
        inverse <- NULL
        set <- function(y)
        {
                #set the data
                x <<- y
                #set the inverse to null because new data is set
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}

#'Compute the inverse of the matrix using solve 
#'The inverse is returned from cached if data has not changed
#'
#'@author Harvinder Singh
#'
#'@param x is an object of type makeMatrix
#'@return rounded value of inverse
cacheSolve <- function(x,...)
{
        #access the inverse value from object
        inverse <- x$getInverse()
        #if inverse value is already available, return it
        if(!is.null(inverse))
        {
                message("getting cached inverse data")
                return(inverse)
        }
        data <- x$get()
        #compute the value of inverse and round it off
        inverse <- round(solve(data))
        x$setInverse(inverse)
        #return new value
        inverse
}