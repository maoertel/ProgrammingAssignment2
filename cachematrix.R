## Put comments here that give an overall description of what your
## functions do

    # the functions in general do:
    # "makeCacheMatrix" is taken a matrix and implements the setters and getters
    # "cacheSolve" invertes the given matrix and sets the inverted matrix

## Write a short comment describing this function

    # The first function (makeCacheMatrix) take an invertible matrix as input argument
    # - in the first line the vaiable m is set to NULL, what is important for the check 
    #   in the second line of the cacheSolve function (have a look at the if-clause there)
    # - then comes the setter, where you can set a matrix y, which initializes "x" in the cache
    #   with the "<<-" operator and sets "m" to NULL again (thta's improtant if another matrix 
    #   was cached and inverted before)
    # - then the getter is implemented that just returns "x"
    # - afterwards the setter for the "solved" (inverted, so to speak) matrix is implemented
    #   which simply initializes m in the cache with the "<<-" operator
    # - surprise, surprise: the getter for solMat is implemented, which returns "m"
    # - list stands for...

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolMat <- function(solMat) m <<- solMat
    getSolMat  <- function() m
    list(set = set, get = get,
         setSolMat = setSolMat,
         getSolMat = getSolMat)
}

## Write a short comment describing this function

    # The second function (cacheSolve) takes the matrix x as in input argument
    # - first of all the variable "m" is initialized by the getSolMat() function (implemented in makeCacheMatrix)
    # - then the function check via if-clause if the matrix was inverted before (that is checked by the variable 
    #   "m" --> if it is not NULL it was inverted before and then you get cached data, what is printed as a 
    #   message to the console)
    # - then a variable data is initialized by the getter function (of makeCacheMatrix)
    # - finally "m" is initialized by the "solved" (inverted) data-variable
    # - then "m" is set again via the setSolMat function (from makeCacheMatrix)
    # - finally "cacheSolve" returns "m" the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolMat()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setSolMat(m)
    m
}