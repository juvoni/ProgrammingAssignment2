# @name makeCacheMatrix
# @description Create a Matrix Object to cache it's Inverse
# @param  x default Matrix
# @return List of public functions: setters and getters
#/
makeCacheMatrix <- function(x = matrix()) {
       	_inverse <- NULL
        set <- function(y) {
                x <<- y
                _inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) _inverse <<- inverse
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# @name cacheSolve
# @description Get the inverse of a Matrix
# @param  x
# @return Matrix Inverse Object
#/
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        _inverse <- x$getInverse()
        if (!is.null(_inverse)) {
                message('Retreiving cached data...')
                return(_inverse)
        }
        _matrix <- x$get()
        _inverse <- solve(_matrix, ...)
        x$setInverse(_inverse)
        _inverse
}
