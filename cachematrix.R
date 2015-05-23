## Gives the inverse of matrix with cache layer,
## so it can save up calculation time over multiple responses.

## Stores cache matirx

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	set <- function(value) {
		x <<- value
		inverse <<- NULL
	}

	get <- function() {
		x
	}

	setInverse <- function(inverse) {
		inverse <<- inverse	
	}

	getInverse <- function() {
		inverse
	}

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return inverse of matrix, from cache, if possible

cacheSolve <- function(x, ...) {
	solved <- x$getInverse()
	if (!is.null(solved)) {
		message("inverse data from cache!")
		return(x)
	}

	value <- x$get()

	solved <- solve(value)

	x$setInverse(solved)

	solved
}
