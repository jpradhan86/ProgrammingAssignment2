## makeCacheMatrix will cache the inverse so that if you call the cacheSolve 
## for the same matrix again it will return the cached inverse instead of 
## calculating it again

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


## Calculate and return inverse of the matrix if it is new otherwise return the cached inverse

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
