## Caching the Inverse of a Matrix


## A special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        n <- NULL
        set <- function(t) {
                x <<- t
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## A function that computes the inverse of the special "matrix" returned by 
## makeCacheMatrix

casheSolve <- function(x, ...) {
        n <- x$getinverse
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
        
        ## Return a matrix that is the inverse of 'x'
}