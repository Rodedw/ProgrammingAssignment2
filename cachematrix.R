## This function will take advantage of the scoping rules of the R 
## language to avoid potentially time-consuming computations. It will
## compute the inverse of a matrix and cache the result for future use
## rather than recompute the value. 


## The first function, makeCacheMatrix creates a list containing 
## functions to set and get the value of the matrix, and set and 
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL 
    set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
      }

    get <- function() x 			# return the input matrix
    setInv <- function(inv) xinv <<- inv 	# set the inversed matrix
    getInv <- function() xinv 			# return the inversed matrix
      
    list(set = set, get = get,
	 setInv = setInv,
	 getInv = getInv)
}


## This function calculates the inverse of a matrix created with the above
## function. If the the inverse was already calculated, it will get the 
## value from teh cache and avoids the recomputation. 

cacheSolve <- function(x, ...) {
      m <- x$getInv() 		# get the inversed matrix from object x
      if(!is.null(m)) {
	  message("getting cached data")
	  return(m) 
      }

      data <- x$get() 
      m <- solve(data) 		#solve it if is not cached
      x$setInv(m) 
      m 
}
