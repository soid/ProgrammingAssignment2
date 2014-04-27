## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix = NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(value) inverseMatrix <<- value
        getInverseMatrix <- function() inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
