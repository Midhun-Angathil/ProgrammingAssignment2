## These are functions that accept a matrix as an input and 
## calculate it's inverse if it hasn't been calculated already.
## The result is then cached to retrieve the same in future
## if a request to compute is made with the same input.

## The following function accepts a matrix and stores it.
## This also does the work of displaying the matrix on the console
## and showing the inverse of it if it exists, else it displays NULL.

makeCacheMatrix <- function(x = matrix()) {

		i <- NULL
		set <- function(y){
			 x <<- y
			 i <<- NULL
		}
	get <- function() x
	set_inv <- function(inv) i <<- inv
	get_inv <- function() i
	list(set = set, get = get, set_inv = set_inv, get_inv = get_inv) 

}


## The following function first checks for inverse that was already
## calculated earlier. If it finds one, it returns the same else it 
## calculates the inverse of the matrix and returns the result to 
## set_inv() in the previous function. 

cacheSolve <- function(x, ...) {

		i <- x$get_inv()
		if(!is.null(i)) {
				message("getting cached data")
				return(i)
		}
	data <- x$get()
	i <- solve(data, ...)
	x$set_inv(i)
	i		## Return a matrix that is the inverse of 'x'
}	











