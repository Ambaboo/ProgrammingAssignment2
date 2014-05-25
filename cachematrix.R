## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL	# set the archetype
	set <- function(y) {	# set the value of the matrix
		x <<- y
		i <<- NULL
	}
	
	get <- function() x	# get the value of the matrix
	setinverse <- function(inverse) i <<- inverse	# set the value of the inverse
	getinverse <- function() i	# get the value of the inverse
	list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()	# get the inverse
		if(!is.null(i)) {	# send a message if the data is not empty
		        message("getting cached data")
		        return(i)
		}
		data <- x$get()	# get the data required
		m <- solve(data, ...)	# calculate the inverse of the matrix
		x$setinverse(i)
		i	# output the inverse
}
