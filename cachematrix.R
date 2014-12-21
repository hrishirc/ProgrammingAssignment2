## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function which returns a list of matrix 'functions' to make,
## get, set (using cacheSolve) and get inverse of the matrix which preferably
## is given during makeCacheMatrix function invovation. You can also create an
## empty makeCacheMatrix() and then use the set function with a square matrix
## inside. 

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- x
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Obvious I feel... it checks using the list containing operations on the
## matrix given as input and calculates the inverse if the environment in which
## the inverse variable was defined has a value attached to it (by previous
## invocation of cacheSolve of course)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m)
        x$setinverse(i)
        i
}
