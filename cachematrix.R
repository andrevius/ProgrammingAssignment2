## These function create a matrix with a cached inverse,
## using the x and x_inv variables in the environment
## where these functions are defined. (In other words,
## if x and/or x_inv are tampered with by other code,
## the assumptions of these functions are broken.)

## This functions creates a list of functions used to 
## set and retrieve a (singleton) matrix and its cached
## inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv = NULL
    set <- function(y) { x<<-y; x_inv<<-NULL}
    get <- function() x
    set_inv <- function(inv) {x_inv <<-inv}
    get_inv <- function() x_inv
    list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## This function returns the inverse of the matrix x,
## returning the cached version if already computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (is.null(inv)) {
        data = x$get()
        inv <- solve(data,...)
        x$set_inv(inv)
    }
    inv
}
