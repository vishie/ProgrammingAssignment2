## Programming Assignment 2 : Lexical Scoping for the Coursera Course:
## The R Programming Language
## @author: Viswanath Ramachandran

## makeCacheMatrix stores the inverse of a matrix in the environment variable inv
## and returns a list that contains the functions to set and get the matrix 
## as well as set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a cached matrix and returns the inverse cached
## if not cached, it then computes the inverse and caches it in the environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
