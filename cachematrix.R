# In this assignment we use the <<- operator to assign a matrix to an object in an 
# environment that is different from the current environment. Below are two functions 
# that are used to create a special object that stores a matrix and cache the inverse matrix.
# 
# The first function, makeCacheMatrix creates a special "matrix", which is really a list 
# containing a function to
# 
#     set the matrix 
#     get the matrix
#     set the inverse matrix
#     get the inverse matrix

makeCacheMatrix <- function(inputMatrix = numeric()) {
    inverseMatrix <- NULL 
    set <- function(y) {
        inputMatrix <<- y      # set the inputMatrix with y input value
        inverseMatrix <<- NULL  # set the default NULL to the inverse Matrix
    }
    
    get <- function() inputMatrix  # get the Matrix
    
    setinverse <- function(inverseM) inverseMatrix <<- inverseM   # set the inverse Matrix
    getinverse <- function() inverseMatrix   # get the inverse Matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The following function calculates the inverse matrix with solve() function. However, 
# it first checks to see if the inverse matrix has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, 
# it calculates the inverse matrix and sets the value of the inverse matrix in the cache via 
# the setinvers function.

cacheSolve <- function(x, ...) {
    
    if(!is.null(x$getinvers())) {  # check inverse matrix is not null
        message("getting cached inverse Matrix")
        return(x$getinvers())
    }
    else {         # if inverse matrix is null
        matrix <- x$get()  
        inverseMatrix <- solve(matrix)  # calculate inverse marix
        x$setinverse(inverseMatrix)     # set the inverse matrix for the object
        inverseMatrix                   # retrun inverse matrix
    }
    
}

########################### Test Case  ####################################
# 
# 
# TestMatrix = matrix( c(2,4,3,1,5,7,7,4,2),3,3)
# 
# TestMatrix
# 
# # [,1] [,2] [,3]
# # [1,]    2    1    7
# # [2,]    4    5    4
# # [3,]    3    7    2
# 
#     
# TestObject<- makeCacheMatrix(TestMatrix)  # call constructor makeCacheMatrix() to instantiate a test Object
# 
# TestObject$get()    # get the matrix
# TestObject$getinverse()   # get the inverse matrix which is null
# 
# 
# cacheSolve(TestObject)  # call cacheSolve first for setting and getting inverse matrix
# 
# cacheSolve(TestObject)  # call again cacheSolve for getting inverse matrix from the cache
# 
# TestObject$get()  
# 
# ###################### Matrix * Inverse Matrix ##################
# ResultMatrix <- TestObject$get()   %*% cacheSolve(TestObject)
# 
# ####  of using the following statement
# ResultMatrix <- TestObject$get()   %*% TestObject$getinverse()
# ####===========================================================
# 
# ResultMatrix <- round(ResultMatrix)
# 
# ResultMatrix
# 
