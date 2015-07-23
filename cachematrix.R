## This is a "Assignment: Caching the Inverse of a Matrix".
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a special "vector", which is a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	iM <- NULL
	set <- function(y) {
        	x <<- y
        	iM <<- NULL
	}
	get <- function() x
	setinvM <- function(invM) iM <<- invM
	getinvM <- function() iM
	list(set=set, get=get, setinvM=setinvM, getinvM=getinvM)
}

## cacheSolve returns the inverse Matrix, after checking if the inverse has already been computed or not.

cacheSolve <- function(x, ...) {
	iM <- x$getinvM()
	if(!is.null(iM)) {
                 message("getting cached data")
                 return(iM)
	}
	data <- x$get()
	iM <- solve (data)
	x$setinvM(iM)
	iM
}
