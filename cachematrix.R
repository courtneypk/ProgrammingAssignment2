## The purpose of the code in the script is to enable a user to access a cached version of 
## the inverse of a square matrix if it exists and, if not, to calculate the inverse and return it
## to the user. 
## 
## There are two kinds of common errors that could result in the user receiving an error code from
## R. The first occurs if the user uses a non-square matrix; we are told for this assignment to assume that 
## all matrices used are square ones. The second occurs if the matrix is singular; that is if the 
## determinant of the matrix is '0'. If this is the case, the inverse of such a matrix is
## undefined and the attempt calculate it would result in an error. In order to avoid the second error and to
## provide meaningful error messages, extra code is present in the cacheSolve function below.
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## This function, makeCacheMatrix, is an R object that defines four methods related to matrices
## and their inverses:
##
##  1. set(m) takes a matrix as its argument
##  2. get() returns the matrix stored in the object
##  3. setInverse(i) takes the inverse of the matrix as its argument
##  4. getInverse() returns the inverse of the matrix, if it is available
##
## These methods are returned as a list of functions. When this function is assigned to a
## variable, say 'z', the methods are implemented as follows:
##
## z$set(m)
## z$get()
## z$setInverse(i)
## z$getInverse()
##

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL      
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}

##
## cacheSolve is used to obtain the inverse of a matrix that has already been stored in a local
## variable in the makeCacheMatrix environment. However, the function first checks to see if 
## the inverse of the matrix already exists by calling the function, x$getInverse(), defined 
## in makeCacheMatrix. If the inverse exists, the inverse is returned. If the inverse does not
## exist, the matrix is retrieved using x$get(). But before using solve() to find the inverse,
## the code checks to see if the matrix is singular by calculating the determinant. If the 
## matrix is singular, a message is passed back to the user. If not, the inverse is returned  
## to the user.
##

cacheSolve <- function(x, ...) {  
  ## Return a matrix, 'i', that is the inverse of 'm'. The parameter, 'x', for this function is
  ##  x <- makeCacheMatrix()
  
  i <- x$getInverse()
  if(!is.null(i)) { ## If 'i' is not null, then return i
    message("Getting cached inverse of matrix")
    return(i)
  }
  message("Creating inverse of matrix")
  m <- x$get()
  if(determinant(m) == 0) {
    message("This is a singular matrix that has no inverse. Please use a non-singular matrix and try again.")
  }
  else{
    i <- solve(m, ...)
    x$setInverse(i) ## use setInverse to cache the inverse for the next time.
    i    
  }
}
