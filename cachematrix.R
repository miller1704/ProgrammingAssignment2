##The first function, makeCacheMatrix creates a special list containing a function to

##set the value of the Matrix
##get the value of the Matrix
##set the value of the Inverse Matrix
##get the value of the Inverse Matrix


makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvm  <- function(inversematrix) invm <<- inversematrix
        getinvm <- function() invm
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)

}


##The following function calculates the Inverse Matrix of the invert-able Matrix created with the above function. 
## However, it first checks to see if the Inverse Matrix has already been calculated. 
## If so, it gets the Inverse Matrix from the cache and skips the computation. 
##Otherwise, it calculates the Inverse Matrix of the data and sets the value of the Inverse Matrix in the cache via the setinvm function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##
        invm <- x$getinvm()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinvm(invm)
        invm
}


