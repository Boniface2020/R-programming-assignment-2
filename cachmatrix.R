## cached matrix inversion functions for 
## programming assignment 2
## demonstrates lexical scoping

## creates a matrix with getters and setters
## so regetting of an inverse matrix gets
## result form cache instead of recalculating

makeCacheMatrix <- function(mat = matrix()) {
    # initialize the inverse to NULL
    inv <- NULL
    
    # setter for matrix
    set <- function(mtrx) {
        mat <<- mtrx
        inv <<- NULL
    }
    
    # getter for matrix
    get <- function() mat
    
    # setter for the inverse
    set.inverse <- function(setinv) inv <<- setinv
    
    # getter for the inverse
    get.inverse <- function() inv
    
    # returns a list of getter and setter functions
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## cache solves the inverse of a matrix
## if already calculated returns cached
## results instead

cacheSolve <- function(cached.mat, ...) {
    # get the stored inverse
    inv <- cached.mat$get.inverse()
    # if it exists return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # else calculate, store and return it
    raw.mat <- cached.mat$get()
    inv <- solve(raw.mat, ...)
    cached.mat$set.inverse(inv)
    
    # returns the inverse
    inv
}
