##  Written by pjimen5 
##  These functions: 1) Create an object matrix that can cache its inverse and 2) Compute the inverse of this type of matrices.

## makeCacheMatrix function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mInv <- NULL
 	set <- function(y) {
   		 x <<- y
  		 mInv <<- NULL
 	}
	get <- function() x
	setInverseMtx <- function(solve) mInv <<- solve
	getInverseMtx <- function() mInv
	list(set = set, get = get,
       	setInverseMtx = setInverseMtx,
 	getInverseMtx = getInverseMtx)
}


## cacheSolve function computes the inverse of the special “matrix” returned my makeCacheMatrix

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     mInv <- x$getInverseMtx()
     if(!is.null(mInv)) {
         message("Getting the inverse matrix from cached data")
         return(mInv)
     }
     data <- x$get()
     mInv <- solve(data, ...)
     x$setInverseMtx(mInv)
     mInv

}
