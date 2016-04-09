## These functions are designed to take place of the "expensive" process of inverting a matrix every time it is requested. By creating and saving its inverse that set of operations will only need to happen once. 


## This function creates a special matrix which caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix  = getmatrix)
}

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

