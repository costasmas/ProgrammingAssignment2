##The functions below will be used to calculate and 
##store in cache an inversed version of a given matrix


## makeCacheMatrix function creates a list 
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        set <- function(y) {
			x <<- y
			inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) {
						inverse <<- inv
					}
        getinverse <- function() {
					   inverse
					}			 
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve checks if the inverse is calculates and  returns it
## if it hasn't been calculated then uses the solve function to create it, 
## stores it, with the stInverse, and returns it

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("Load data from cache.")
                return(inverse)
        }
        y <- x$get()
        inverse <- solve(y, ...)
        x$setinverse(inverse)
        inverse
}
