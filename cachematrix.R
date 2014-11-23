## The following function takes an inversible matrix as an argument.
## Then the variable "i" is initiated, followed by four functions.  The results
## of the functions are added to a list so that they can be called later on (see example below
## the function "cacheSolve").
## Description of the four functions within the makeCacheMatrix function:
## set: sets the values of the matrix
## get: gets the values of the matrix
## setinverse: sets the values of the inversed matrix
## getinverse: gets the values of the inversed matrix
makeCacheMatrix <- function(x = matrix()){
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function(){
		x
	}
	setinverse <- function(inverse){
		i <<- inverse
	}
	getinverse <- function(){
		i
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will check if the inversed matrix already exists in cache
## If so, a message "getting cached data" is displayed
## and the inversed matrix is returned.  Otherwise, the inverse of the matrix
## is calculated and saved in cache.
cacheSolve <- function(x, ...){
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

my.input <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
mymatrix <- makeCacheMatrix(my.input)
mymatrix$get()
mymatrix$getinverse()
cacheSolve(mymatrix)
mymatrix$get()
mymatrix$getinverse()
cacheSolve(mymatrix)