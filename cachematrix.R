

## Two functions to avoid waste time calculating repeteadly (in this case inverse of matrix) if we have calculated this item previosly


## makeCacheMatrix makes a list of the functions and stores to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                           #creates the first function (set the "value")
                x <<- y
                m <<- NULL
        }
        get <- function() x                            #creates  the second function (get the "value")
        setsolve <- function(solve) m <<- solve        #creates the third function (setsolve)
        getsolve <- function() m                       #creates the fourth function (getsolve)
        list(set = set, get = get,                     #stores in a list all functions
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve  is a function that calculates the inverse it it's not in cache where looks for the result first

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()                                           #gets de value of cache to m
        if(!is.null(m)) {                                           # if it has been calculated previosly returns its value
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                             #gets de value of the matrix
        m <- solve(data, ...)                                       #aply the function solve (inverse)
        x$setsolve(m)                                               #stores in cache for next result  
        m                                                           #returns result
}
