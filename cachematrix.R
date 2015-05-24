## Put comments here that give an overall description of what your
## functions do
#
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverter <- function(inverse) inv <<- inverse
    getinverter <- function() inv
    list(set = set, get = get,
         setinverter = setinverter,
         getinverter = getinverter)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverter()
        if(!is.null(inv)){
            message("getting cached data")
            retrun(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinverter(inv)
        return(inv)
}
