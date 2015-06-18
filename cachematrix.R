## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL

	set <- function(m){
		x <<- m
		inv_x <<- NULL
	}

	get <- function() x

	set_inverse <- function(m){
		inv_x <<- m
	}

	get_inverse <- function() inv_x

	list(set = set,
		get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$get_inverse()
        if(!is.null(inv_x)){
        	message("Getting inverse from Cache")
        	return(inv_x)
        }

        m <- x$get()

        inv_x <- solve(m, ...)

        x$set_inverse(inv_x)

        inv_x
}
