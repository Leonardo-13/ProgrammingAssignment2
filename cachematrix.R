## Put comments here that give an overall description of what your
## functions do:
## The two functions below interact with each other: 'makeCacheMatrix' takes a
## square invertible matrix as input/argument and ultimatly creates a list object that can store and redefine both the
## is capable of storing (get) or redefining (set) the contents of the such square
## invertible matrix. Similarly, 'makeCacheMatrix' also stores and retrieves the
## inverse matrix of the original one, provided it has been calculated by the function
## 'cacheSolve'

## Write a short comment describing this function

## This function defines 6 objects: x,m, set(), get(), setinverse() and getinverse()
## the 'x' is meant to be passed on the value of a square invertible matrix
## the 'm' is initialized as NULL
## 'set()' and 'get()' are functions that become attributes of the object created 
## by 'makeCacheMatrix', which are capable of setting or retrieving (respecitvely)
## the matrix that is stored by the variable 'x'
## 'setinverse() and 'getinverse()' act in the same way that 'set()' and 'get()'
## but they set or retrieve the inverse value of the matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(SomeMatrix){
                x <<- SomeMatrix
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrizinversa) m <<- matrizinversa
        getinverse <- function() m
        list(SetMatrix = set, GetMatrix = get, SetMatrixInverse = setinverse, GetMatrixInverse = getinverse)
        
}

## Write a short comment describing this function
## This function takes the list object created by the function above. First it
## uses its 'GetMatrixInverse()' attribute to return the value of the matrix inverse
## that is stored by the function 'makeCacheMatrix' and assigns it to the variable
## 'm'. 
## Then it checks whether 'm' is null or not. If the value of 'm' is not null, 'cacheSolve'
## will return the message "getting cached data" and right afterwards print on screen
## the value of  'm'
## If 'm' however does turn out to be null, it will proceed to do the following
##  -> Use the 'GetMatrix()' attribute of the object that was entered as argument
##     to retrieve the stored matrix, and assign it to the variable 'matriz'
##  -> Calculate the inverse matrix of 'matriz' and assign it to the variable 'm'
##  -> Use the 'SetMatrixInverse()' attribute of the object which was entered as argument
##     to redefine the inverse matrix value in that object to the one that the 
##     variable 'm' from the step above carries.
##  -> Finally, simply return to the screen the value of 'm', which is inverse of 
##     the matrix calculated 2 steps above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$GetMatrixInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matriz <- x$GetMatrix()
        m <- solve(matriz)
        x$SetMatrixInverse(m)
        m
}
