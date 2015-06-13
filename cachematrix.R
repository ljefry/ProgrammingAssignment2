# The following functions perform the following
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


# makeCacheMatrix takes a square invertible matrix and creates a matrix object 
# with a list of functions focusing on getting & setting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_matrix_inverse <- function(matrix_inverse) m <<- matrix_inverse
    get_matrix_inverse <- function() m
    list(set = set, get = get,
         set_matrix_inverse = set_matrix_inverse,
         get_matrix_inverse = get_matrix_inverse)
}


# cacheSolve uses the matrix object created by makeCacheMatrix to: 
# 1. Return the cached matrix inverse
# OR 
# 2. Calculates the matrix inverse, sets the matrix inverse and then returns the matrix inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_matrix_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$set_matrix_inverse(m)
    m
}
