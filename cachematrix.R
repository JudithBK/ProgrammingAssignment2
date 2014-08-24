## Overall description of the two functions: 
## makeCacheMatrix does....
## cacheSolve does....


## Creates a matrix that is the inverse of X (does the actual caching)

makeCacheMatrix <- function(x = matrix()) {
## x is the matrix you want to work with
    
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ##this takes the vector or matrix y, and saves it under "x". This leaves us with x that we can manipulate, while y is protected/not touched
    
    get <- function() x
    ## if you call the get function, it will just provide you with the value of x
    setinverse <- function(solve) m <<- solve
    ##setinverse is the function that does the actual inversion
    
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
## can't use $ here, unless I have names in the matrix, which I do not...
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
