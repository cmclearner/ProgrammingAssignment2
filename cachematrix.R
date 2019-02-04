## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
	# initialize the inverse
	i <- NULL
	# set the matrix
	set <- function(matrix) {
		x <<- matrix
		i <<- NULL
	}
	# get the matrix
	get <- function(){
		# return the matrix
		x
	}
	# set the inverse of the matrix
	setinverse <- function(inverse){
		i <- inverse
	}
	# get the inverse of the matrix
	getinverse <- function(){
		# return the inverse
		i
	}
	# return a list of the methods
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## writing the function for cacheSolve

cacheSolve <- function(x, ...){
	# return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	# return the inverse if it is already set
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	# get the matrix from our object
	data <- x$get()
	# calculate the inverse using matrix multiplication
	m <- solve(data) %*% data
	# set the inverse to the object
	x$setinverse(m)
	# return the matrix
	m
}


