## -----------------------------------------------------------------------
## Author       Gerald P. Wright
##
## Language     R 3.2.0
## File         cachematrix.R
##
## Description: 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.
##
## The functions in this file cache an inverse matrix.
##
## Inverted matrix:
## Given the square matrix (n x n) A, then the inverse matrix A^(-1)
## such that A * A^(-1) = I, where I is the Identity matrix of size
## n x n
##
## example
##      1   3   3
## A = [1   4   3]
##      1   3   4
##
##      1   3   3               7   -3  -3      1   0   0
## A = [1   4   3] x A^(-1) [   -1  1   0] = [  0   1   0]
##      1   3   4               -1  0   1       0   0   1
##
## end header ------------------------------------------------------------

## -----------------------------------------------------------------------
## Functions:
##
## makeCacheMatrix
## this function creates a special "vector", which is realy a list 
## containing functions to (in order)
##      - set the value of the cached matrix
##      - get the value of the cached matrix
##      - set the inverse of the matrix
##      - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## default cache to NULL
    inverseMatrix <- NULL
    
    ## set the value of the cached matix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## get the value of the cached matrix
    get <- function() x
    
    ## set the inverse of the matrix
    ## uses R function solve
    setInverse <- function(solve) inverseMatrix <<- solve
    
    # get the inverse of the matrix
    getInverse <- function() inverseMatrix
    
    # build vector of functions
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
} 

## cacheSolve
## this function will find the compute the inverse matrix
## it will use the vlaue in the cache if the inverse has been previously
## calculated; otherwise, it will compute it
cacheSolve <- function(x, ...) {
    ## get the current inverse matrix from cache
    inverseMatrix <- x$getInverse()
    
    ## if it has been previously calculated
    ## thenjust return it
    if(!is.null(inverseMatrix)){
        message("getting cached data")
        return(inverseMatrix)
    }
    
    ## it has not been previously calculated
    ## so, we must compute it
    matrix <- x$get()
    inverseMatrix<-solve(matrix, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}   
## end functions ---------------------------------------------------------

## -----------------------------------------------------------------------
## Test data:
##
## example:
##      1   3   3
## A = [1   4   3]
##      1   3   4
##
##      1   3   3               7   -3  -3      1   0   0
## A = [1   4   3] x A^(-1) [   -1  1   0] = [  0   1   0]
##      1   3   4               -1  0   1       0   0   1
##
## ----------
## run through:
## setwd("~/GitHub/ProgrammingAssignment2")
## source("cachematrix.R")
##
## example <- matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4), nrow = 3, ncol = 3)
## 
## cache = makeCacheMatrix(example)
## cache$get()
## cacheSolve(cache)
## cacheSolve(cache)
## 
## ----------
## sample output:
## m <- matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4), nrow = 3, ncol = 3)
## c = makeCacheMatrix(m)
## 
## c$get()
## [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4
## 
## cacheSolve(c)
## [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## 
## cacheSolve(c)
## getting cached data
## [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
## end test data ---------------------------------------------------------
