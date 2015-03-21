##makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
##set the value of the inverse, get the value of the inverse, set the value of the solve,
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


## cacheSolve function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the matrix and sets the value of it in the cache via the setsolve function

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
