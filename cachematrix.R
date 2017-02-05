## This fuction is to calculate the inverse of the matrix. If it's called for 2nd time it will getting 
## the cache data instead of recaluate the function again.
##-------------------------------------------------------------------
## This Fuction will create the list of the functions that store the assigned data. 
##-------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This fuction will calcualte the inverse of a matrix and return the cache value if it's called for the 2nd time. 
##-------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
