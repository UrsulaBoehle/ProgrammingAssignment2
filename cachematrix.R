# The point of this function - apart from seeing the concept of closures
# and lexical scoping in real practice, is to make us aware the saving
# computing power by caching previously calculated vales instead of 
# constantly re-calculating them is necessary for efficiently running code.
# In my personal case it also had the positive side effect of forcing
# me to go back to my high school days and refresh my knowledge of linear
# algebra.

# The following function is meant to create a special "matrix" object 
# that will cache it's inverse.
# As a result it will create  a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of inverse of the matrix
# 4) get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL #sets the variable to NULL as a placeholder 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # defines a function to set the vector, x, to a new vector, y, 
        # and resets inv to NULL
        get <- function() x
        # returns x
        setInverse <- function(solve) inv <<- solve #calculate the inverse
        getInverse <- function() inv # returns inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        # returns all the functions defined in a list
}

# The function cacheSolve following here returns the inverse of the matrix. 
# First, it finds out if the inverse of the matrix has already been computed. 
# In the positive case, it gets the result and stops. 
# In the negative case, it computes the inverse of the matrix, 
# and sets the value in the cache via the setinverse function.


cacheSolve <- function(x=matrix(), ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}

# Seems to work according to my tests - hope you agree:
# my_matrix$set(matrix(c(6,6,3,1),2,2)) 
# my_matrix$get()
# [,1] [,2]
# [1,]    6    3
# [2,]    6    1
# > my_matrix$getInverse()
# NULL
# > cacheSolve(my_matrix)
# [,1]  [,2]
# [1,] -0.08333333  0.25
# [2,]  0.50000000 -0.50
# > cacheSolve(my_matrix)
# getting cached data.#  
# [,1]  [,2]
# [1,] -0.08333333  0.25
# [2,]  0.50000000 -0.50
# my_matrix$getInverse()
# [,1]  [,2]
# [1,] -0.08333333  0.25
# [2,]  0.50000000 -0.50
