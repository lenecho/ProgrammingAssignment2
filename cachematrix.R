##########################################################
##Programming assignment #2
##########################################################



## Create an R function called makeCacheMatrix which defines a list of subfunctions. 
## makeCacheMatrix contains a list of four subfunctions:
##		1)The get function returns the matrix x stored in the main function (makeCacheMatrix).
##		2)The set function can change the matrix x stored in the main function, hence the x <<- y. Otherwise this substitution could only take place within the set subfunction. 
##				If there is a substitution, any previously calculated inv is set to NULL
##		3)The setinverse function can be used to set the inverse
##		4)The getinverse function returns the inverse



makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}
## Create a new function, cacheSolve, which may be linked to the makeCacheMatrix function. When running the cacheSolve function, you will get the inverse of any square, invertible matrix that you may specify.
## If the inv variable is null(meaning that its value has not yet been calculated and it therefore cannot be cached), the function gets the matrix specified in the makeCacheMatrix 
##		using the get function and calculates the inverse of the given matrix.
##		It then sets the inverse in the cache memory, and returns the inverted matrix.
## If the inv variable differs from null, the function prints the message "getting cached data" and returns the previously calculated inv from cache.
## Care needs to be taken with the ... element of the solve() function and/or the cacheSolve function, as any b matrix or value would not be cached. 
## 		This can potentially yield the wrong result. It should be left as ..., alternatively b could be set to 1.



cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

		
		

}


### Example showing the use of the cacheSolve function, using cached information when available.

##Specify the matrix as a 2000x2000 matrix of random values and assign makeCacheMatrix function to this matrix, thereby assigning all four list functions under makeCacheMatrix to this object.
##Store this as the object foo in the global environment.
foo <- makeCacheMatrix(matrix(rnorm(2000**2), 2000, 2000))

##Get the inverse of the matrix, calculating the first time. The resulting inverse matrix can now be cached later.
cacheSolve(foo)

##Run the cacheSolve function again. This time it uses the cached information, and returns the result much quicker. We have also instructed to print the message "getting cached data" when using cached data.
##This will print the second time we run cacheSolve(foo). 
cacheSolve(foo)

##If you want to look at the original matrix, use the get subset of the makeCacheMatrix function:
foo$get




