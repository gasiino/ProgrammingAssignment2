## These functions can create the inverse of a matrix ( which is required to be 
## invertible) and store in a cache, where it can be retrieved without 
## calculating it again.
## Please note: at the bottom of the file there are a few sample lines of test.
## The code in this file has been tested using these simple tests.

## This function returns a list of 4 functions: 
## set, get, getinverse, setinverse.
## Using these functions you can get the original matrix, or set it again, get 
## the inverse matrix, or set it again
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## setup of the 4 functions
    ## 1 set is the function to update the matrix with a new value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## 2 get is the function to return the current matrix value
    get <- function() x
    
    ## 3 setinverse is the function to update the value of the inverse matrix 
    ## with a new value    
    setinverse <- function(solve) m <<- solve
    
    ## 4 getinverse is the function to return the inverse matrix    
    ## please note: if the inverse has not already been calculated, getinverse 
    ## will return null
    getinverse <- function() m
    
    ## creation of the list of the 4 functions to be returned
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function retrieves the inverse from the cache if its available
## otherwise calculates the inverse matrix and returns it
cacheSolve <- function(x, ...) {
    ## create the inverse matrix by calling the getinverse function from x
    m <- x$getinverse()
    
    ## if m is not null then x contains a cached version of the inverse matrix
    if(!is.null(m)) {
        message("getting cached data.")
        ## returning the cached version
        return(m)
    }
    ## if m is null then x doesn't contain a cached version of the inverse 
    ## matrix.
    
    message("cache is empty. populating cache.")
    ## To update the cacche and return the inverse matrix:
    ## 1. get the actual original matrix
    data <- x$get()
    
    ## 2. create the inverse matrix. 
    m <- solve(data, ...)
    
    ## 3. update x putting the inverse matrix in the cache
    x$setinverse(m)
    
    ## 4. return the inverse matrix
    m
}


## simple tests
# amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get()         # Returns original matrix
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# amatrix$getinverse()  # Returns matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
# amatrix$get() %*% cacheSolve(amatrix) # matrix multiplication returns identity
# 
# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# amatrix$get()         # Returns matrix
# amatrix$getinverse()  # Returns matrix inverse
# amatrix$get() %*% cacheSolve(amatrix) # matrix multiplication returns identity
# 
# cmatrix <- makeCacheMatrix(matrix(c(1,2,3,4,5,6,7,8,10),nrow = 3,ncol = 3))
# cacheSolve(cmatrix) # Computes, caches, and returns new matrix inverse
# cmatrix$get()
# cmatrix$getinverse()
# cmatrix$get() %*% cacheSolve(cmatrix) # matrix multiplication returns identity
# 
