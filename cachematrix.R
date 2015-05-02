## This function includes a pair of two functions that cache the inverse of a matrix
## By calling this function, the costly computation caused by repeatedly computing the inverse of a matrix can be avoided.


## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL        # set the inv to be NULL
        set <- function(y) {
          x <<- y          # change and set the matrix and its inverse stored in the function 
          inv <<- NULL
        }
        get <- function() x    # return the matrix stored in the main function
        setinv <- function(solve) inv <<- solve   #set the value inverse as the inverse of the matrix
        getinv <- function() inv    # get the inverse of the matrix
        list(set = set, get = get,  # store the four subfunctions in the main function
            setinv = setinv,
            getinv = getinv)
}



## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()     # get the previously stored inverse matrix
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)         # if there was an inverse matrix stored, return the inverse matrix
        }
        data <- x$get()       # if not, retrieve the matrix
        inv <- solve(data, ...)    # calculate the inverse of the matrix
        x$setinv(inv)              # set the calculated inverse matrix to the matrix
        inv
}
