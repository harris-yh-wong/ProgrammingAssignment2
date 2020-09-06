## Put comments here that give an overall description of what your
## functions do

# These functions aim to cache the value of the inverse of a matrix, which 
# is useful for large matrices.
# The 'makeCacheMatrix' function creates an R object that stores a vector and
# its mean. This R object contains a complete copy of the environment from
# makeVector().
# 
# This R object is then used as an input argument into the 'cacheSolve'
# function. This function is where the solve() function is executed. It first
# checks whether the inverse has already been calculated. If so, it retrieves
# the mean from the cahce; otherwise, it calculates the inverse and sets the
# value of the inverse.


## Write a short comment describing this function
#
# This function creates a special "matrix" object that can cache its inverse.
#
# Firstly, the function initiates objects x and inv.
# Next, the function builds four "methods", set(), get(), setInverse() and 
# getInverse().
# Then, the function returns these four methods as a list to the parent
# environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # define mutator and accessor methods
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(computedInverse) inv <<- computedInverse
        getInverse <- function() inv
        
        # return list
        list <- list(set = set,
                     setInverse = setInverse,
                     get = get,
                     getInverse = getInverse)
}

## Write a short comment describing this function
#
# This function returns first checks whether the inverse is cached. If the 
# inverse has already been computed and that the matrix has not changed, then
# the function retrieves the cached inverse. Otherwise, it calculates inverse
# by calling solve() then calles setInverse(). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # check cache
        inv <- x$getInverse()
        
        # if yes: retrieve inverse
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # if no: calculate inverse, call setInverse()
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
