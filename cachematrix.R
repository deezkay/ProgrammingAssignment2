## -------------------------------------------------------------------------------------
## These functions provide a mechanism to return the inverse of a matrix
## Instead of repeatedly calculating the inverse of the same matrix
## the function caches the inverse result in the global environment using 
## the <<- assignment operator
##
## To test these functions run the following:-
##    m <- matrix(c(1,1,1,3,4,3,3,3,4), 3, 3)
##    z <- makeCacheMatrix(m)
##    cacheSolve(z)
##        should return the following:-
##                  [,1] [,2] [,3]
##            [1,]     7   -3   -3
##            [2,]    -1    1    0
##            [3,]    -1    0    1
##    Running cacheSolve(z) should display 'getting cached inverted matrix'
##
## -------------------------------------------------------------------------------------
## makeCacheMatrix()
## This function accepts a matrix parameter and store it internally
## The function returns a list of internal functions for setting & getting the matrix
## and setting & getting the inverted matrix
## These functions are used by the cacheSolve() function below
makeCacheMatrix <- function(m = matrix()) {
    ## default variable initialisation
    inv_m <- NULL
  
    ## matrix set function (note global environment assignment)
    set <- function(x) {
        m <<- x
        inv_m <<- NULL
    }
  
    ## matrix get function
    get <- function() m
  
    ## inverted matrix set function (note global environment assignment)
    setinv <- function(x) inv_m <<- x
  
    ## inverted matrix get function
    getinv <- function() inv_m
  
    ## return a list of the 4 internal functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## -------------------------------------------------------------------------------------
## cacheSolve()
## This function takes a list previously created using the makeCacheMatrix function
## and calculates the inverse of the previously stored matrix from the global environment
## and then stores (caches) the inverted matrix itself in the global environment
## If the function detects that an inverted matrix is already stored in the
## global environment, it returns that value rather than perform a re-calculation
cacheSolve <- function(x, ...) {
    ## get the inverted matrix value
    inv_m <- x$getinv()
  
    ## check if it already has a value (cached)
    ## in which case we can return the cached value
    if (!is.null(inv_m)) {
        message("getting cached inverted matrix")
        return(inv_m)
    }
  
    ## not cached so we need to get original matrix
    ## and calculate the matrix inversion
    m <- x$get()
    inv_m <- solve(m, ...)
  
    ## store (cache) the result 
    x$setinv(inv_m)
  
    ## return the result
    inv_m
}

