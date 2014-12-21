## PROGRAMMING ASSIGNMENT 2 - R Programming  Course
## This script contains a pair of functions that are used to create a special
## object that stores a matrix and caches its inverse.


## makeCacheMatrix: this function takes a matrix 'x' as argument and returns a
## list containing the results of the following functions: 
## 1. set, to set the matrix, 
## 2. get, to get the matrix, 
## 3. setinvm, to compute and cache the inverse matrix, and 
## 4. getinvm, to get the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize 'm' to NULL value
        m <- NULL
        ## Define the 'set' function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Define the 'get' function
        get <- function() x
        ## Define the 'setinvm' function and compute the inverse of 'x' matrix
        setinvm <- function(solve) m <<- solve
        ## Define the 'getinvm' function 
        getinvm <- function() m
        ## Return the results of the previous functions in a list
        list(set = set, get = get,
                setinvm = setinvm,
                getinvm = getinvm)
}


## cacheSolve: this function checks if the inverse of the matrix 'x' has already
## been calculated by the function 'makeCacheMatrix'. If yes, it retrieves the
## inverse matrix from the cache, prints a message, and skips the computation. 
## If no, it calculates the inverse matrix and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Retrieve the 'getinvm' element from the list and store it in 'm'
        m <- x$getinvm()
        ## Check if the inverse matrix has already been calculated and 
        ## is stored in 'm'
        if(!is.null(m)) {
                ## If yes, return the inverse matrix 'm' from the cache 
                ## and exit the function
                message("getting cache data")
                return(m)
        }
        ## Get the matrix 'x'
        data <- x$get()
        ## Compute the inverse of the matrix 'x' and store it in 'm'
        m <- solve(data, ...)
        ## Store the inverse matrix 'm' in the list
        x$setinvm(m)
        ## Return the inverse matrix 'm'
        m
}
