## Two functions are created for the purpose of caching the inverse of 
## a matrix. For this purpose, first a special "matrix" is created that
## can cache its own inverse. Secondly, a function is created to compute
## the inverse of the special "matrix" created using the first function.

## The following function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	a <- NULL
	set <- function(y) {
		x <<- y
		a <<- NULL
		}
	get <- function(){ 
		x
		}
	setinverse <- function(inverse){
		 a <<- inverse
		}
	getinverse <- function(){ 
		a
		}
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function computes the inverse of the special "matrix"
## created by the above function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	a <- x$getinverse()
	if(!is.null(a)){
	message("getting cached data")
	return(a)
	}
	mat <- x$get()
	a <- solve(mat,...)
	x$setinverse(a)
	a
}
