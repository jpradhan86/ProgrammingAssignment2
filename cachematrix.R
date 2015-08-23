## Put comments here that give an overall description of what your
## functions do

## Create the matrix and Catch the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
        set <- function(y) {
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) I <<- solve
	getInverse <- function() I
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	I <- x$getInverse()
	if(!is.null(I)) {
		message("getting cached data")
		return(I)
	}
	data <- x$get()
	I <- solve(data, ...)
	x$setInverse(I)
	I
}
