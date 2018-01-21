## Since matrix inversion is a costly computation, it is often good to cache the inverse of the matrix to
## avoid recomputation for further use ahead in the program
## functions do

## makeCacheMatric is a function contains a List that cache the inverse of the matrix computed by CacheSolve it also 
## returns the value of inverse to cacheSolve when requested

makeCacheMatrix <- function(x = matrix()) {
        
        inv <-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
  
}


## This  function computes the inverse of the matrix and parse it to makeCacheMatrix for caching. It also
## retrived the alredy cached value of inverse from makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
