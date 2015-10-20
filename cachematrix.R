## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL # assign null to name variable of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse() # try to get inverse matrix from cache
        # if m is cached already just return it ...
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # return inverse matrix from cache
        }
	# ... if m was empty obtain original matrix and write into
	# variable "data"
        data <- x$get()	
        m <- solve(data) # then get inverse of matrix data and write to m
        x$setinverse(m) 
        m # return inverse matrix from data


}

## RUNNING A SESSION WITH THE CODE

## read file
#> source('cachematrix.R')
## Create a simple 3x3 matrix.
#> A=makeCacheMatrix(matrix(c(1,0,1,0,2,1,1,1,1),nrow=3,ncol=3))
## Check whether the matrix was created correctly:
#> A$get()
#     [,1] [,2] [,3]
#[1,]    1    0    1
#[2,]    0    2    1
#[3,]    1    1    1

# Try getting the inverse of A:
#> A$getinverse()
#NULL

## It failed because the code looked for it in the cache. The inverse
## of A has not been created yet because "cacheSolve" has not been run 
## yet.

## run cacheSolve(A)to obtain the inverse of A:
#> cacheSolve(A)
#     [,1] [,2] [,3]
#[1,]   -1   -1    2
#[2,]   -1    0    1
#[3,]    2    1   -2

# Repeating the command now yields the inverse of A:
#> A$getinverse()
#     [,1] [,2] [,3]
#[1,]   -1   -1    2
#[2,]   -1    0    1
#[3,]    2    1   -2

## Repeating cacheSolve now retrieves the inverse of A from cache:
#> cacheSolve(A)
#getting cached data
#     [,1] [,2] [,3]
#[1,]   -1   -1    2
#[2,]   -1    0    1
#[3,]    2    1   -2
