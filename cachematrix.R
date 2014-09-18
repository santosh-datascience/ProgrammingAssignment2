## For a large dimension matrix, computing its inverse is time consuming.  
## If the contents of the matrix have not changed, then it makes sense to 
## cache the inverse of the matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 

## The function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inv) inverseMatrix <<- inv
        
        getInverse <- function() inverseMatrix
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the matrix. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <-x$getInverse()
        
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        
        origMatrix <- x$get()
        inverseMatrix <- solve(origMatrix)
        x$setInverse(inverseMatrix)
        
        inverseMatrix
}


## Sample output: Unit test
## > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##> amatrix$get()
##        [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(amatrix) 
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> amatrix$getinverse()
##> amatrix$getInverse()
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(amatrix) 
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
##> cacheSolve(amatrix)
##      [,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0
##> amatrix$get()
##      [,1] [,2]
##[1,]    0   99
##[2,]    5   66
##> amatrix$getInverse()
##      [,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0