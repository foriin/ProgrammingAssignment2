##makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
##set the value of the inversed matrix, get the value of the inversed matrix, set the value of the solve,
##get the value of the solve


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
cacheSolve(makeCacheMatrix(x)) #x is non-singular matrix
