## These two functions serve the purpose of creating a "special" matrix object
## containing a (invertible and square) matrix and its inverse; the matrix object
## is represented as a list

## makeCacheMatrix takes a matrix and returns a new matrix object; this matrix object
## has a set, get, setinverse, and getinverse function. set and get are functions to
## to set / change and retrieve the current matrix, respectively; setinverse and
## getinverse are functions to set and retrieve the inverse matrix of the current matrix, 
## respectively. These functions can be called via the $-operator

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y # sets x in the makeCacheMatrix-environment to y
            i <<- NULL # sets i in the makeCacheMatrix-environment to NULL
      }
      get <- function() x # gets the current matrix
      setinverse <- function(inverse) i <<- inverse # sets i in the makeCacheMatrix-environment 
                                                    # to the inverse matrix of the current matrix
      getinverse <- function() i # gets the current inverse matrix
      return(list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)) # returns a "special" matrix object (as a list)

}


## This function takes a makeCacheMatrix object as input and checks if the inverse 
## was already computed (i.e., is already cached). If it was already computed, the cached
## inverse matrix is returned. If it was not already computed, the inverse matrix of
## the special matrix object is computed, cached via the setinverse() setter and then
## returned

cacheSolve <- function(x, ...) {
      i <- x$getinverse() # gets the current inverse for the special matrix object x
      if(!is.null(i)) {
            message("getting cached matrix inverse") # pretty self-explanatory
            return(i) # function ends here if inverse is already in cache
      }
      # the following lines are only executed if the current inverse of x is a NULL object
      data <- x$get() # get the current data matrix
      i <- solve(data, ...) # solve for the inverse matrix
      x$setinverse(i) # cache the inverse matrix
      return(i) # return the inverse matrix
}

# test case from discussion forum
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)