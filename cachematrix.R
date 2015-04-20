## makeCacheMatrix creates a list containing four functions
##      1.)  set -- used to set the value of the matrix as well as the cache value
##                  and is only used to change the matrix after makeCacheMatrix 
##                  is initallized. 
##      2.)  get -- retuns the value of the matrix 
##      3.)  setinverse -- used to save the inverse of the matrix 
##      4.)  getinverse -- retuns the value of the inverse of the matrix 
## 
## cacheSolve uses the function created in makeCacheMatrix to caluclate the inverse
##      of the matrix as well as save the inverse in cache.


## 'x' is a matrix which will be inverted.   

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 'x' is a list of functions created in makeCacheMatrix.   

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
