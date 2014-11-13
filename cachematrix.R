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

## Write a short comment describing this function
## This function will check if the inversed matrix already exist in cache
## If so, message "getting cached data" is displayed
## and the inversed matrix is returned.
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