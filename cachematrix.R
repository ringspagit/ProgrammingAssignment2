#################################################################################################
## Project	: Programming Assignment 2
## Author	: Paul Ringsted
## Date		: 2018-09-27
## Description	: Functions to define a special version of a matrix, calculate and cache the inverse
## Usage Ex.	: > A <- matrix(1:4,2,2)	-- set up basic 2x2 test matrix 'A'
##		  > cA <- makeCacheMatrix(A)	-- initialize cached matrix 'cA' from 'A'
##		  > iA <- cacheSolve(cA)	-- calculate inverse matrix 'iA' from 'cA'
##		  > cA$get()			-- returns cached matrix (=A)
##		  > cA$getinv()			-- returns cached inverse (=iA)
##		  > A %*% iA			-- returns identity matrix
##		  > cA$get() %*% iA		-- returns identity matrix
##		  > A <- matrix(4:1,2,2)	-- change matrix
##		  > cA$set(A)			-- update cache.  inverse reset to NULL until cacheSolve() called
#################################################################################################

## makeCacheMatrix
## Takes a matrix as argument and initializes special version of the matrix
## Returns a list with the following function elements:
##	set	Sets the special matrix with the matrix provided
##	get	Returns the current value of the special matrix
##	setinv	Caches the inverse of the special matrix with the matrix provided
##	getinv	Gets the cached inverse of the special matrix, or NULL if not cached

makeCacheMatrix <- function(x = matrix()) {

	# Initialize the inverse to NULL
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i

	#Return list of functions for this special matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## Takes a special matrix 'x' previously initialized by makeCacheMatrix()
## Returns the inverse of the matrix 'x', calculated if not cached

cacheSolve <- function(x, ...) {

	# Get the cached version (if set) from the special matrix
	i <- x$getinv()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	# If no cached inverse available, calculate it with solve()
	data <- x$get()
	i <- solve(data,...)

	# Set the inverse in the cache and return the inverse matrix
	x$setinv(i)
	i
}
