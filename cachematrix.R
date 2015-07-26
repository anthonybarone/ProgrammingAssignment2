## cachematrix.R consists of a pair of functions that compute & cache the 
## inverse of a matrix.

## makeCachematrix creates a special "matrix" object 
## that can cache its inverse.

##NOTE:  Following same pattern in the Example: Caching the Mean of a Vector supplied.
##NOTE:  Primary change was caching the Mean of a MATRIX (a two dimensional data structure in R) 
##NOTE:  instead of a VECTOR (a one demensional data structure in R)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Computing the inverse of a square matrix can be done with the solve function in R. 
        ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("LET'S SPEED IT UP... GETTING CACHED DATA!!!!")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}


## To run a script, first make sure you are in the correct directory by running list.files()
## Once in correct directory with source file pass a string with its name to the source function. 
## e.g. Try running the "cachematrix.R" script by running command > source("cachematrix.R")

## EXAMPLE RUN USING THE COMMAND LINE:

## rbind note: Takes a sequence of matrix arguments and combine by columns or rows, respectively.
## > x = rbind(c(2, -1/2), c(-1/2, 2))

## makeCacheMatrix(x) note:  passing in the matrix x argument defined above 
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]  2.0 -0.5
## [2,] -0.5  2.0

## cacheSolve(m) note:  passing in the matrix m agrument defined above.  No cache in the first run, got NULL
## > cacheSolve(m)
## > cacheSolve(m)
##      [,1]      [,2]
## [1,] 0.5333333 0.1333333
## [2,] 0.1333333 0.5333333

## cacheSolve(m) note:  Retrieving from the cache in the second run
## > cacheSolve(m)
## LET'S SPEED IT UP... GETTING CACHED DATA!!!!
##      [,1]      [,2]
## [1,] 0.5333333 0.1333333
## [2,] 0.1333333 0.5333333