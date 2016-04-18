## makeCacheMatrix returns a a list,
## containing the following functions:
##  set()		set the value of the matrix
##  get()		get the value of the matrix
##  setinverse()	cache the inverse of the matrix
##  getinverse()	get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
      	x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created
## with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
