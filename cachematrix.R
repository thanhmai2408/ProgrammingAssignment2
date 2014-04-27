## Put comments here that give an overall description of what your
## functions do
#The files contain two functions
#1: function makeCacheMatrix() to create a special matrix which hold the matrix itself and its inverse value
#2: function cacheSolve() to calculate the inverse of the matrix

## Write a short comment describing this function
## This function is used to create a special matrix which holds the matrix itself and its inverse value
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL #inv holds the inverse value of the x matrix
	set <- function(y) { #set function which receives y as an input
		x <<- y 
		inv <<- NULL #re-assign any value (if any) of the inverse to NULL because the new matrix has been SET
	}
	get <- function() x #a function to return the current matrix
	setInverse <- function(inverse) inv <<- inverse #a function to set the inverse of a matrix. It is used to retrieve the inverse of the matrix in cacheSolve function
	getInverse <- function() inv #a function to get the current inverse of a matrix
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse) #create a list which holds four functions
}


## Write a short comment describing this function
## This function return a matrix which is the inverse of a matrix x 
## It will first check if the inverse of x has been calculated. If yes, it will return the inverse of the matrix. If not, it will calculate
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse() ##check if the inverse of matrix x has been calculated
	
	#if the inverse of x has been calculated, then getting the inverse from the cache
	if(!is.null(inv)) { 
		message("getting cached data")
		return(inv)
	}
	
	#if the inverse of x has not been calculated, then calculate
	data <- x$get() #retrieve the matrix
	inv <- solve(data, ...) #compute the inverse matrix
	x$setInverse(inv) #save the inverse matrix
	inv #return the inverse matrix
}