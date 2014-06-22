## makeCacheMatrix creates a special list object that contains functions to modify a matrix,
## and cacheSolve is a function that can query the object and set its inverse, testing to
## see if it already has a value for the inverse (and thus just grabbing that)

## this function creates a special list holding the matrix, and the inverted matrix from the
## input matrix 'x'. It has a bunch of set and get commands.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <- NULL #reset inverse to null to force a recalc inverse
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function first tests to see if the makeCacheMatrix object has an inverted matrix value,
## if it doesn't then it will calulate one and set it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) { #if inverse exists already then just return it
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data) #otherwise recalc and set inverse.
        x$setinverse(inverse)
        inverse
}
