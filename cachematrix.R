## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function makeVector creates a special "vector", which is really a list 
##containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function()x
        setinverse <- function(solvedinverse) inverse<<- solvedinverse
        getinverse <- function()inverse
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function
##The following function calculates the mean of the special "vector" created with the 
##above function. However, it first checks to see if the mean has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and 
##sets the value of the mean in the cachevia the setmean function.

cacheSolve <- function(x, ...) {
        inverse <- x$getmean()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solvedinverse(data, ...)
        x$setmean(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
