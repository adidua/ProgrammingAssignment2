## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix", which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeMatrix <- function(x = matrix()) {
	xInv <- NULL
	set <- function(y) {
		x <<- y
		xInv <<- NULL
	}
	get <- function()x
	setInv <- function(inv) xInv <<- inv
	getInv <- function() xInv
	list(set = set, get = get,
	     setInv = setInv,
	     getInv = getInv)			
}


## This function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to 
## see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the value of the
## inverse in the cache.

cacheSolve <- function(x, ...) {
	x
	xInv <- x$getInv()
	if (!is.null(xInv)) {
		message("getting cached data")
		return(xInv)
	}
	data <- x$get()
	xInv <- solve(data, ...)
	x$setInv(xInv)
	xInv
}
