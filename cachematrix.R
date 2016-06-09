## The makeCacheMatrix function takes a matrix as an input and stores it. The cacheSolve function takes
## a matrix as input and calls the getinverse function to check if the inverse is cached or not. 
## If it is not cached then it goes ahead calculates the inverse using the solve function and then
## caches the result using the setinverse function. On the first call of cacheSolve function it 
## it calculates the inverse of the matrix and caches it. On the subsequent calls to the cacheSolve
##function with the same matrix the function fetches the cached data and returns it to the screen.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) m <<- inverse
        getinverse<- function() m
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse= getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
