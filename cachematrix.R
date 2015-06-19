## makeCacheMatrix takes an input as a square invertible matrix and caches the inverse of the matrix.
## The function returns a list used as the input to the companion function cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
## x: a square invertible matrix
## return: a list containing functions to
##	1. set the matrix
##	2. get the matrix
##	3. set the inverse
##	4. get the inverse
##	this list is used as the input to cacheSolve()

	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
		}
	get <- function() x
	setinv <- function(solve) inv <<- solve 
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve takes the output of makeCacheMatrix and returns the inverse matrix from the cache.
## If the inverse has not been cached, the function should calculate the inverse and return the inverse.

 cacheSolve <- function(x, ...) {
		## x: output of makeCacheMatrix()
		## returns: inverse of the original matrix input to makeCacheMatrix()
         
		inv <- x$getinv()
         
		## if the inverse has already been calculated
         if (!is.null(inv)){
                 ## gets inverse from the cache and skips the computation. 
                 message("getting cached data")
                 return(inv)
         }
         
         ## otherwise the function calculates the inverse 
         data <- x$get()
         inv <- solve(data, ...)
         
         ## sets the value of the inverse in the cache via the setinv function.
         x$setinv(inv)
         
         return(inv)
 }
