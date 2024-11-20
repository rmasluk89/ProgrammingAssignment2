################################################################################

## makeCacheMatrix is a function that:
##
## 1.  Takes an optional argument x, which defaults to an empty matrix if not
##     provided.
## 2.  Initializes a variable inv to NULL which will be used to store the 
##     inverse of the matrix.
## 3.  Defines four inner functions:
##              set(y):  This function updates the matrix x with a new value y
##                       and resets inv to NULL.  The <<- operator is used for
##                       assignment in the parent environment.
##              get():  This function returns the current value of x.
##              setinv(inv):  This function sets the value of inv to the 
##                            provided inverse.
##              getinv():  This function returns the current value of inv.
## 4.  Returns a list containing the four inner functions.

################################################################################

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

################################################################################

## cacheSolve is a function designed to work with the matrix object created by
## makeCacheMatrix.  Its purpose is to calculate and cache the inverse of the
## matrix, returning the cached value if it has already been calculated.
## The function:
##
## 1.  Takes an argument x, which is expected to be a matrix object created by
##     makeCacheMatrix, and the (...) allows for additional arguments to be 
##     passed to the solve function.
## 2.  Tries to get the inverse using x$getinv().  This calls the getinv 
##     function defined in makeCacheMatrix.
## 3.  If the inverse (inv) is not NULL, it means that a cached value exists.
##     In this case, it prints the message noting that it is using cached data
##     and returns the cached inverse.
## 4.  If the inverse is NULL (and therefore not cached), it proceeds to 
##     calculate the inverse using the following steps:
##      4a.  It gets the matrix data using x$get().
##      4b.  It calculates the inverse using solve(data, ...).
##      4c.  It sets this calculated inverse in the matrix object using 
##           x$setinv(inv).
##      4d.  Finally, it returns the calculated inverse.

################################################################################

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
