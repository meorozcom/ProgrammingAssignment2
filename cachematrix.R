## makeCacheMatrix() creates a special "matrix", which is really a list containing
## a function to set the value of a matrix, get the value of the matrix, set the 
## value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(r){
                x<<- r
                m<<- NULL
        }
        get <- function()x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve() calculates the mean of the special "matrix" created before with 
## the makeCacheMatrix() function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
