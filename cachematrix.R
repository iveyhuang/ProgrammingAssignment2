## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object 
    ## that can cache its inverse.
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if (dim(data)[1] == dim(data)[2]) {
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
    } else {
        message("Error: X is not a square matrix.")
    }
    
    
    
}

# mymatrix <- rbind(c(1, -1/4), c(-1/4, 1)); mymatrix
# solve(mymatrix)
# 
# mycachmatrix <- makeCacheMatrix(mymatrix)
# # class(mycachmatrix)
# # mycachmatrix$getinverse()
# # mycachmatrix$get()
# cacheSolve(mycachmatrix)