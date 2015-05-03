## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creates a special matrix, which is a list containing a function to
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of the inverse of the matrix
# 4 - get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {        # defines matrix as 'x'
        invmatrix <- NULL                          # the inverse matrix defined as 'invmatrix' is empty
        set <- function(y) {                       # sets the function to matrix 'x'
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x                               # gets the value of the matrix'x' that will be inverted
        setinv <- function(inverse) invmatrix <<- inverse # sets the value of the inverted matrix 'invmatrix'
        getinv <- function() invmatrix                    # gets the value of the inverted matrix 'invmatrix'
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
## FUNCTION 2: 'cacheSolve'                                       #
## This function returns the inverse of a matrix by:              #
## 1. Verifying if the matrix has already been inverted.          #
## 2. If yes, then the inverted matrix is returned.               #
## 3. If no, then the raw data is retrieved, inverted, and cached #
##    before returning the newly inverted matrix.                 #


cacheSolve <- function(x, ...) {
       invmatrix <- x$getinv()                 #retrieves the inverse of the matrix
        if(!is.null(invmatrix)) {              #checks if the matrix has already been inverted
                message("getting cached data") #notifies user with a message
                return(invmatrix)              #returns inverted matrix
        }
        data <- x$get()                        #if the matrix is not inverted, then function retrieves raw data
        invmatrix <- solve(data, ...)          #inverts the matrix 
        x$setinv(invmatrix)                    #sets the inverted matrix equal to invmatrix
        invmatrix
}

}
