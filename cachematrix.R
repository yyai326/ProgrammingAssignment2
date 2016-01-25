
## makeCacheMatrix function to cache the Matrix inversion
## return list contain set,get,setinv,getinv 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #set Matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get Matrix
    get <- function() x
    #set Matrix inversion
    setinv <- function(inverse) inv <<- inverse
    #get Matrix inversion
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



##cacheSolve function checks whether an inverted matrix has already been cached
## if not, calculate the Matrix inversion and store in cache
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
