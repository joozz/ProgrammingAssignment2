## The makeCacheMatrix creates a matrix object that caches its inverse. In general, if A is original matix and B its inverse, 
## following condition is satisfied: AB = I, where I is an "identity matrix". 

## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse matrix has 
## already been calculated, cacheSolve will retrieve this from cache (in order to save computation)

## makeCacheMatrix contains four functions: set, get, setinv, getinv. 
## 1) set set is a function that changes the vector stored in the main function.  "x <<- y" provides for substitution 
## of the vector x with the input y in the main function makeCacheMatrix. 
## 2) get is a function that returns the vector x stored in the main function. get requires no input.
## 3) setinv stores the value of the input (invmatrix) into m 
## 4) getinv returns the value of input m 

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL 
    set <- function(y) {
        x <<- y
        m <<- NULL 
    }
    get <- function() x 
    setinv <- function(invmatrix) m <<- invmatrix
    getinv <- function() m 
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## cacheSolve function returns a matrix that is the inverse of 'x'.  cacheSolve checks if the inverse matrix has 
## already been calculated and matrix has not changed - and if available returns the content of the cache. In this case
## the message "getting cached data" will be printed out to the console. If cache is not available solve() calculates
## the inverse. Note that it is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


