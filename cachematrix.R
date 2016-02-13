## two functions that cache the inverse of a matrix

## create a special matrix object that can cache its inverse
## returns the created matrix

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
        set <- function(y){
          x <<- y
          x.inv <<- NULL
        }
        get <- function() x
        setInv <- function(invMatrix) x.inv <<- invMatrix
        getInv <- function() x.inv
        list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## if not in the cache, this function computes
## the inverse of the special matrix returned by the function above
## the input matrix is assumed to be invertible

cacheSolve <- function(x, ...) { 
       ## Return a matrix that is the inverse of 'x'
       x.inv <- x$getInv()
       if(!is.null(x.inv)) {
           return(x.inv)
      }else{
          mat <- x$get()
          x.inv <- solve(mat) #this will work only with an invertible matrix, which the assumtion here
          x$setInv(x.inv)
          return(x.inv)
      }
}


## a simple test
## a <- cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
## a %*% matrix(1:4,2,2)
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
       

