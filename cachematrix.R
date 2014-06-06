## The first function, makeCacheMatrix:
## 1) Sets value of matrix
## 2) Gets value of matrix
## 3) Sets value of inverse
## 4) Gets value of inverse
##
## The second function, cacheSolve, calculates the inverse of the vector created above.
## It first checks to see if the inverse has already been calculated.
## If the inverse has been calculated, it gets the value from the cache.
## If not, it will calculate the inverse and puts the inverse in the cache for later use.

## Creates special "matrix" that can cache its inverse

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


## Checks to see if inverse has been calculated. If it has, obtains that value from cache.
## If not, calculates inverse and caches inverse for later use.

cacheSolve <- function(x, ...) {
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
