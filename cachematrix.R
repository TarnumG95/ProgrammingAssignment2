
## Make a matrix containing its cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    make <- function(y) {
        x <<- y
        inv <<- solve(y)
    }
    get <- function() {
        x
    }
    getinv <- function() {
        inv
    }
    setinv <- function(i) {
        inv <<- i
    }
    list(make = make, get = get, getinv = getinv, setinv = setinv)

}


## Find inverse of a matrix

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    i = x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    d <- x$get()
    i = solve(d)
    x$setinv(i)
    i
}

test = matrix(1:4, 2, 2)
a = makeCacheMatrix(test)
cacheSolve(a)