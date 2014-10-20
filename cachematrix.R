## Two functions to create a matrix capable of caching its inverse and solving the inverse
## itself. Assumes that the matrix is solvable.

## Create a matrix that can cache its inverse. On creation the inverse is not calculated.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        set_matrix_inverse<-function(inverse) mat <<- inverse
        get_matrix_inverse<-function() mat
        list(set = set, get = get, set_matrix_inverse = set_matrix_inverse, get_matrix_inverse = get_matrix_inverse)
        
}


## Solve and return the inverse of a matrix and cache the result. 

cacheSolve <- function(x, ...) {
        mat <- x$get_matrix_inverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        mat <- x$get()
        mat <- solve(mat, ...)
        x$set_matrix_inverse(mat)
        mat
}
