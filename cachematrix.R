## Using the list created with the function "makeCacheMatrix", "cacheSolve"
## returns the stored inverse of the matrix if it exists, or computes it
## from scratch

## This function creates a list with four elements

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## the original matrix is assigned to "get"
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv ## "inv" is assigned to "getinverse"
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If "inv" already had a stored value, that value is returned. If that's not the case,
## it is computed. The original matrix is assigned to "data", and
## "solve(data)" returns the inverse of that matrix, which is stored as the setinverse" element
## in the "x" list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }
}
